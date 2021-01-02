// Package evaluator ...
package evaluator

import (
	"fmt"
	goast "go/ast"
	goparser "go/parser"
	gotoken "go/token"
	"reflect"
	"strconv"
	"strings"

	"lukechampine.com/slouch/ast"

	"github.com/traefik/yaegi/interp"
	"github.com/traefik/yaegi/stdlib"
)

type Value interface {
	isValue()
	fmt.Stringer
}

type IntegerValue struct {
	i int64 // TODO: make this arbitrary-precision
}

func (iv IntegerValue) isValue()       {}
func (iv IntegerValue) String() string { return strconv.FormatInt(iv.i, 10) }

type StringValue struct {
	s string
}

func (sv StringValue) isValue()       {}
func (sv StringValue) String() string { return strconv.Quote(sv.s) }

type BoolValue struct {
	b bool
}

func (bv BoolValue) isValue()       {}
func (bv BoolValue) String() string { return strconv.FormatBool(bv.b) }

type ArrayValue struct {
	elems []Value
}

func (av ArrayValue) isValue() {}
func (av ArrayValue) String() string {
	strs := make([]string, len(av.elems))
	for i := range strs {
		strs[i] = av.elems[i].String()
	}
	return "[" + strings.Join(strs, ", ") + "]"
}

type PartialValue struct {
	fn   Value
	args []Value // missing values represented by nil
}

func (pv PartialValue) have() int {
	n := 0
	for _, a := range pv.args {
		if a != nil {
			n++
		}
	}
	return n
}

func (pv PartialValue) isValue() {}
func (pv PartialValue) String() string {
	return fmt.Sprintf("<partial (%d-adic, have %v)>", len(pv.args), pv.have())
}

type BuiltinValue struct {
	name  string
	nargs int
	fn    func(*Environment, []Value) Value
}

func (bv BuiltinValue) isValue() {}
func (bv BuiltinValue) String() string {
	return fmt.Sprintf("<builtin %q (%d-adic)>", bv.name, bv.nargs)
}

type GoValue struct {
	name string
}

func (gv GoValue) isValue()       {}
func (gv GoValue) String() string { return gv.name }

type Environment struct {
	gointerp *interp.Interpreter
	idents   map[string]Value
}

func (env *Environment) addFuncSymbols(snippet string) {
	f, err := goparser.ParseFile(gotoken.NewFileSet(), "_.go", "package foo\n\n"+snippet, 0)
	if err != nil {
		panic(err)
	}
	for _, dec := range f.Decls {
		if fn, ok := dec.(*goast.FuncDecl); ok {
			env.idents[fn.Name.Name] = GoValue{fn.Name.Name}
			// TODO: include fn metadata (nargs)
			// TODO: maybe just convert to FnValue?
		}
		// TODO: add variable decls?
	}
}

func (env *Environment) Run(p ast.Program, input string) error {
	env.idents["input"] = StringValue{input}
	for _, n := range p.Stmts {
		switch n := n.(type) {
		case ast.SnippetStmt:
			if _, err := env.gointerp.Eval(n.Code); err != nil {
				return err
			}
			env.addFuncSymbols(n.Code)
		case ast.AssignStmt:
			// if the assignment is to input, and the result is a
			// partially-applied function missing its last argument, supply
			// input as the final argument
			v := env.Eval(n.X)
			if pv, ok := v.(PartialValue); ok && n.Name.Name == "input" {
				v = env.apply(pv.fn, []Value{env.idents["input"]})
			}
			env.idents[n.Name.Name] = v
		case ast.ExprStmt:
			// if the result is a partially-applied function missing its last argument,
			// supply input as the final argument
			v := env.Eval(n.X)
			if pv, ok := v.(PartialValue); ok {
				v = env.apply(pv.fn, []Value{env.idents["input"]})
			}
			fmt.Println(v)
		default:
			panic(fmt.Sprintf("unknown stmt type %T", n))
		}
	}
	return nil
}

func (env *Environment) Eval(e ast.Expr) Value {
	switch e := e.(type) {
	case ast.String:
		return StringValue{e.Value}
	case ast.Integer:
		i, _ := strconv.ParseInt(e.Value, 10, 64) // later: handle bigints
		return IntegerValue{i}
	case ast.Ident:
		if v, ok := env.idents[e.Name]; ok {
			if bv, ok := v.(BuiltinValue); ok {
				return PartialValue{
					fn:   bv,
					args: make([]Value, bv.nargs),
				}
			}
			return v
		}
		panic("unknown identifier " + strconv.Quote(e.Name))
	case ast.InfixOp:
		// convert to partial
		pv := PartialValue{
			fn:   infixFns[e.Op],
			args: make([]Value, 2),
		}
		if e.Left != nil {
			pv.args[0] = env.Eval(e.Left)
		}
		if e.Right != nil {
			pv.args[1] = env.Eval(e.Right)
		}
		if pv.have() == len(pv.args) {
			return env.apply(pv.fn, pv.args)
		}
		return pv
	case ast.FnCall:
		fn := env.Eval(e.Fn)
		args := make([]Value, len(e.Args))
		for i, a := range e.Args {
			// TODO: check for _ here
			args[i] = env.Eval(a)
		}
		return env.apply(fn, args)
	case ast.Pipe:
		l, r := env.Eval(e.Left), env.Eval(e.Right)
		switch r := r.(type) {
		case PartialValue:
			if pv, ok := l.(PartialValue); ok {
				// whole expression becomes a partial
				return PartialValue{
					fn: BuiltinValue{
						name:  "pipepartial",
						nargs: len(pv.args),
						fn: func(env *Environment, args []Value) Value {
							return env.apply(r, []Value{env.apply(pv.fn, args)})
						},
					},
					args: pv.args,
				}
			}

			return env.apply(r, []Value{l})
		default:
			panic(fmt.Sprintf("unhandled pipe RHS %T", r))
		}
	case ast.Array:
		a := ArrayValue{make([]Value, len(e.Elems))}
		for i := range a.elems {
			a.elems[i] = env.Eval(e.Elems[i])
		}
		return a
	default:
		panic(fmt.Sprintf("unhandled node type %T", e))
	}
}

func (env *Environment) apply(fn Value, args []Value) Value {
	switch fn := fn.(type) {
	case BuiltinValue:
		// TODO: check for _ args
		if len(args) < fn.nargs {
			pv := PartialValue{
				fn:   fn,
				args: make([]Value, fn.nargs),
			}
			copy(pv.args, args)
			return pv
		} else if len(args) > fn.nargs {
			panic(fmt.Sprintf("%v expected %v args, got %v", fn.name, fn.nargs, len(args)))
		}
		return fn.fn(env, args)

	case PartialValue:
		if fn.have()+len(args) > len(fn.args) {
			panic(fmt.Sprintf("too many arguments supplied to function: %v expects %v, got %v", fn.fn, len(fn.args), fn.have()+len(args)))
		}
		pv := fn
		pv.args = append([]Value(nil), fn.args...)
		for i := range pv.args {
			if pv.args[i] == nil && len(args) > 0 {
				pv.args[i], args = args[0], args[1:]
			}
		}
		if pv.have() < len(pv.args) {
			return pv // still a partial
		}
		return env.apply(pv.fn, pv.args)

	case GoValue:
		// TODO: need to handle arg passing
		v, err := env.gointerp.Eval(fn.name + "()")
		if err != nil {
			panic(err)
		}
		switch v.Kind() {
		case reflect.Int:
			return IntegerValue{v.Int()}
		case reflect.String:
			return StringValue{v.String()}
		default:
			panic("unhandled return type")
		}
	default:
		panic(fmt.Sprintf("unhandled fn type: %T", fn))
	}
}

func New() *Environment {
	i := interp.New(interp.Options{GoPath: "/Users/luke/go"})
	i.Use(stdlib.Symbols)
	if _, err := i.Eval(`import "fmt"`); err != nil {
		panic(err)
		// } else if _, err := i.Eval(`import "lukechampine.com/advent/utils"`); err != nil {
		// 	panic(err)
	}
	idents := make(map[string]Value, len(builtins))
	for _, b := range builtins {
		idents[b.name] = b
	}
	return &Environment{
		gointerp: i,
		idents:   idents,
	}
}
