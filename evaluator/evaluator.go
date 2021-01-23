// Package evaluator ...
package evaluator

import (
	"fmt"
	goast "go/ast"
	goparser "go/parser"
	gotoken "go/token"
	"reflect"
	"strconv"
	"sync"

	"lukechampine.com/slouch/ast"
	"lukechampine.com/slouch/token"

	"github.com/traefik/yaegi/interp"
	"github.com/traefik/yaegi/stdlib"
)

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
			env.idents[fn.Name.Name] = &GoValue{fn.Name.Name}
			// TODO: include fn metadata (nargs)
			// TODO: maybe just convert to FnValue?
		}
		// TODO: add variable decls?
	}
}

func (env *Environment) Run(p ast.Program, input string) error {
	env.idents["input"] = makeString(input)
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
			if pv, ok := v.(*PartialValue); ok && pv.need() == 1 && n.Name.Name == "input" {
				v = env.apply(pv.fn, env.idents["input"])
			}
			if iv, ok := v.(*IteratorValue); ok {
				// variables should always be arrays, not iterators, because we
				// don't want to consume them multiple times
				v = builtinCollect(iv)
			}
			env.idents[n.Name.Name] = v
		case ast.ExprStmt:
			// if the result is a partially-applied function missing its last argument,
			// supply input as the final argument
			v := env.Eval(n.X)
			if pv, ok := v.(*PartialValue); ok && pv.need() == 1 {
				v = env.apply(pv, env.idents["input"])
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
		return makeString(e.Value)
	case ast.Integer:
		i, _ := strconv.ParseInt(e.Value, 10, 64) // later: handle bigints
		return makeInteger(i)
	case ast.Hole:
		return HoleValue{}
	case ast.Ident:
		if e.Name == "true" || e.Name == "false" {
			return makeBool(e.Name == "true")
		} else if v, ok := env.idents[e.Name]; ok {
			if bv, ok := v.(*BuiltinValue); ok {
				if bv.nargs == 0 {
					return env.apply(bv)
				}
				return makePartial(bv, make([]Value, bv.nargs))
			}
			return v
		}
		panic("unknown identifier " + strconv.Quote(e.Name))
	case ast.InfixOp:
		var l, r Value
		if e.Left != nil {
			l = env.Eval(e.Left)
			// short circuit and/or
			if _, lpartial := l.(*PartialValue); !lpartial {
				if e.Token.Kind == token.And && !internalTruthy(l) {
					return makeBool(false)
				} else if e.Token.Kind == token.Or && internalTruthy(l) {
					return makeBool(true)
				}
			}
		}
		if e.Right != nil {
			r = env.Eval(e.Right)
		}

		// if one or both args are partial values, return a partial that
		// inherits their holes
		lpv, lpartial := l.(*PartialValue)
		rpv, rpartial := r.(*PartialValue)
		if lpartial || rpartial {
			var pvargs []Value
			if lpartial {
				pvargs = append(pvargs, lpv.args...)
			} else {
				pvargs = append(pvargs, l)
			}
			if rpartial {
				pvargs = append(pvargs, rpv.args...)
			} else {
				pvargs = append(pvargs, r)
			}
			fn := &BuiltinValue{
				name:  "exprpartial",
				nargs: len(pvargs),
				fn: func(env *Environment, args []Value) Value {
					if lpartial {
						l, args = env.apply(lpv.fn, args[:len(lpv.args)]...), args[len(lpv.args):]
					} else {
						l, args = args[0], args[1:]
					}

					// short circuit and/or
					if _, lpartial := l.(*PartialValue); !lpartial {
						if e.Token.Kind == token.And && !internalTruthy(l) {
							return makeBool(false)
						} else if e.Token.Kind == token.Or && internalTruthy(l) {
							return makeBool(true)
						}
					}

					if rpartial {
						r, args = env.apply(rpv.fn, args[:len(rpv.args)]...), args[len(rpv.args):]
					} else {
						r, args = args[0], args[1:]
					}
					return env.apply(infixFns[e.Op], l, r)
				},
			}
			return makePartial(fn, pvargs)
		}
		// otherwise, just evaluate immediately
		return env.apply(infixFns[e.Op], l, r)
	case ast.FnCall:
		fn := env.Eval(e.Fn)
		args := make([]Value, len(e.Args))
		for i, a := range e.Args {
			args[i] = env.Eval(a)
		}
		return env.apply(fn, args...)
	case ast.Splat:
		// turn n.Fn into a function that takes an array
		fn := env.Eval(e.Fn)
		sfn := &BuiltinValue{
			name:  "splat",
			nargs: 1,
			fn: func(env *Environment, args []Value) Value {
				a, ok := args[0].(*ArrayValue)
				if !ok {
					panic(fmt.Sprintf("splat expected array, got %T", args[0]))
				}
				return env.apply(fn, a.elems...)
			},
		}
		return makePartial(sfn, make([]Value, 1))
	case ast.Rep:
		// turn n.Fn into a function that takes 1 argument and replicates it as
		// many times as necessary
		fn := env.Eval(e.Fn)
		rfn := &BuiltinValue{
			name:  "rep",
			nargs: 1,
			fn: func(env *Environment, args []Value) Value {
				if len(args) != 1 {
					panic(fmt.Sprintf("rep expected exactly 1 argument, got %v", len(args)))
				}
				var nargs int
				switch fn := fn.(type) {
				case *BuiltinValue:
					nargs = fn.nargs
				case *PartialValue:
					nargs = fn.need()
				default:
					panic(fmt.Sprintf("rep expected function, got %T", fn))
				}
				for len(args) < nargs {
					args = append(args, args[0])
				}
				return env.apply(fn, args...)
			},
		}
		return makePartial(rfn, make([]Value, 1))
	case ast.Pipe:
		l, r := env.Eval(e.Left), env.Eval(e.Right)
		switch e.Token.Kind {
		case token.PipeSplat:
			// splat r
			fn := r
			sfn := &BuiltinValue{
				name:  "pipesplat",
				nargs: 1,
				fn: func(env *Environment, args []Value) Value {
					a, ok := args[0].(*ArrayValue)
					if !ok {
						panic(fmt.Sprintf("splat expected array, got %T", args[0]))
					}
					return env.apply(fn, a.elems...)
				},
			}
			return makePartial(sfn, make([]Value, 1))
		case token.PipeRep:
			// rep r
			fn := r
			rfn := &BuiltinValue{
				name:  "piperep",
				nargs: 1,
				fn: func(env *Environment, args []Value) Value {
					if len(args) != 1 {
						panic(fmt.Sprintf("rep expected exactly 1 argument, got %v", len(args)))
					}
					var nargs int
					switch fn := fn.(type) {
					case *BuiltinValue:
						nargs = fn.nargs
					case *PartialValue:
						nargs = fn.need()
					default:
						panic(fmt.Sprintf("rep expected function, got %T", fn))
					}
					for len(args) < nargs {
						args = append(args, args[0])
					}
					return env.apply(fn, args...)
				},
			}
			r = makePartial(rfn, make([]Value, 1))
		}

		switch r := r.(type) {
		case *PartialValue:
			if pv, ok := l.(*PartialValue); ok {
				// whole expression becomes a partial
				pfn := &BuiltinValue{
					name:  "pipepartial",
					nargs: len(pv.args),
					fn: func(env *Environment, args []Value) Value {
						l = env.apply(pv.fn, args...)
						return env.apply(r, l)
					},
				}
				return makePartial(pfn, pv.args)
			}
			return env.apply(r, l)
		default:
			panic(fmt.Sprintf("unhandled pipe RHS %T", r))
		}
	case ast.Array:
		elems := make([]Value, len(e.Elems))
		for i := range elems {
			elems[i] = env.Eval(e.Elems[i])
		}
		a := makeArray(elems)
		if e.Assoc {
			return builtinAssoc(a)
		}
		return a
	default:
		panic(fmt.Sprintf("unhandled node type %T", e))
	}
}

func (env *Environment) apply(fn Value, args ...Value) Value {
	switch fn := fn.(type) {
	case *BuiltinValue:
		if len(args) > fn.nargs {
			panic(fmt.Sprintf("too many arguments supplied to function: %v expects %v, got %v", fn.name, fn.nargs, len(args)))
		} else if len(args) < fn.nargs {
			pargs := make([]Value, fn.nargs)
			copy(pargs, args)
			return makePartial(fn, pargs)
		}
		return fn.fn(env, args)

	case *PartialValue:
		if fn.have()+len(args) > len(fn.args) {
			panic(fmt.Sprintf("too many arguments supplied to function: %v expects %v, got %v", fn.fn, len(fn.args), fn.have()+len(args)))
		}
		pargs := append([]Value(nil), fn.args...)
		for i := range pargs {
			if isHole(pargs[i]) && len(args) > 0 {
				pargs[i], args = args[0], args[1:]
			}
		}
		pv := makePartial(fn, pargs)
		if pv.have() < len(pv.args) {
			return pv // still a partial
		}
		return env.apply(fn.fn, pv.args...)

	case HoleValue:
		if len(args) != 1 {
			panic(fmt.Sprintf("hole expects a single argument, got %v", len(args)))
		}
		return args[0]

	case *GoValue:
		// TODO: need to handle arg passing
		v, err := env.gointerp.Eval(fn.name + "()")
		if err != nil {
			panic(err)
		}
		switch v.Kind() {
		case reflect.Int:
			return makeInteger(v.Int())
		case reflect.String:
			return makeString(v.String())
		default:
			panic("unhandled return type")
		}
	default:
		panic(fmt.Sprintf("unhandled fn type: %T", fn))
	}
}

var valueSlicePool = sync.Pool{
	New: func() interface{} {
		args := make([]Value, 0, 100)
		return &args
	},
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
