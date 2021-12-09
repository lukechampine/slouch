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

func (env *Environment) newScope() *Environment {
	idents := make(map[string]Value, len(env.idents))
	for k, v := range env.idents {
		idents[k] = v
	}
	return &Environment{
		gointerp: env.gointerp,
		idents:   idents,
	}
}

func (env *Environment) Clone() *Environment {
	return env.newScope()
}

func (env *Environment) Run(p ast.Program, input string, output func(Value)) error {
	if _, ok := env.idents["input"]; !ok {
		env.idents["input"] = makeString(input)
	}
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
				v = env.apply1(pv, env.idents["input"])
			}
			// variables should always be arrays, not iterators, because we
			// don't want to consume them multiple times
			v = internalClone(v)
			if it, ok := v.(*IteratorValue); ok {
				v = builtinCollect(it)
			}
			env.idents[n.Name.Name] = v
		case ast.ExprStmt:
			// if the result is a partially-applied function missing its last argument,
			// supply input as the final argument
			v := env.Eval(n.X)
			if pv, ok := v.(*PartialValue); ok && pv.need() == 1 {
				v = env.apply1(pv, env.idents["input"])
			}
			output(v)
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
				return env.apply(bv)
			}
			return v
		}
		panic(fmt.Sprintf("unknown identifier %q", e.Name))
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
	case ast.Lambda:
		sfn := &BuiltinValue{
			name:  "lambda",
			nargs: e.NumArgs(),
			fn: func(env *Environment, args []Value) Value {
				env = env.newScope()
				for i, c := range "xyzabcdefghijklmnopqrstuvw"[:len(args)] {
					env.idents[string(c)] = internalClone(args[i])
				}
				return env.Eval(e.Body)
			},
		}
		return env.apply(sfn)
	case ast.Splat:
		// turn n.Fn into a function that takes an array
		fn := env.Eval(e.Fn)
		sfn := makeBuiltin("splat", func(env *Environment, a *ArrayValue) Value {
			return env.apply(fn, a.elems...)
		})
		return makePartial(sfn, make([]Value, 1))
	case ast.Rep:
		// turn n.Fn into a function that takes 1 argument and replicates it as
		// many times as necessary
		fn := env.Eval(e.Fn)
		rfn := makeBuiltin("rep", func(env *Environment, v Value) Value {
			v = internalClone(v)
			var nargs int
			switch fn := fn.(type) {
			case *BuiltinValue:
				nargs = fn.nargs
			case *PartialValue:
				nargs = fn.need()
			default:
				panic(fmt.Sprintf("rep expected function, got %T", fn))
			}
			args := make([]Value, nargs)
			for i := range args {
				args[i] = v
			}
			return env.apply(fn, args...)
		})
		return makePartial(rfn, make([]Value, 1))
	case ast.Negative:
		return internalNegate(env.Eval(e.Value))
	case ast.Pipe:
		l, r := env.Eval(e.Left), env.Eval(e.Right)
		switch e.Token.Kind {
		case token.PipeSplat:
			// splat r
			fn := r
			sfn := makeBuiltin("pipesplat", func(env *Environment, a *ArrayValue) Value {
				return env.apply(fn, a.elems...)
			})
			r = makePartial(sfn, make([]Value, 1))
		case token.PipeRep:
			// rep r
			fn := r
			rfn := makeBuiltin("piperep", func(env *Environment, v Value) Value {
				v = internalClone(v)
				var nargs int
				switch fn := fn.(type) {
				case *BuiltinValue:
					nargs = fn.nargs
				case *PartialValue:
					nargs = fn.need()
				default:
					panic(fmt.Sprintf("rep expected function, got %T", fn))
				}
				args := make([]Value, nargs)
				for i := range args {
					args[i] = v
				}
				return env.apply(fn, args...)
			})
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
						return env.apply1(r, l)
					},
				}
				return makePartial(pfn, pv.args)
			}
			return env.apply1(r, l)
		default:
			panic(fmt.Sprintf("unhandled pipe RHS %T", r))
		}
	case ast.Array:
		if e.Assoc && len(e.Elems)%2 != 0 {
			panic("assoc: dangling key")
		}
		elems := make([]Value, len(e.Elems))
		var holes []int
		for i := range elems {
			elems[i] = env.Eval(e.Elems[i])
			if isHole(elems[i]) {
				holes = append(holes, i)
			}
		}
		if len(holes) > 0 {
			hs := make([]Value, len(holes))
			for i := range hs {
				hs[i] = HoleValue{}
			}
			return makePartial(&BuiltinValue{
				name:  "arraypartial",
				nargs: len(holes),
				fn: func(env *Environment, args []Value) Value {
					for i := range args {
						elems[holes[i]] = args[i]
					}
					a := makeArray(elems)
					if e.Assoc {
						return builtinAssoc(internalArrayIterator(a))
					}
					return a
				},
			}, hs)
		}
		a := makeArray(elems)
		if e.Assoc {
			return builtinAssoc(internalArrayIterator(a))
		}
		return a
	default:
		panic(fmt.Sprintf("eval: unhandled node type %T", e))
	}
}

func (env *Environment) apply(fn Value, args ...Value) Value {
	switch fn := fn.(type) {
	case *BuiltinValue:
		have := 0
		for _, a := range args {
			if !isHole(a) {
				have++
			}
		}
		if have > fn.nargs {
			panic(fmt.Sprintf("too many arguments supplied to function: %v expects %v, got %v", fn.name, fn.nargs, have))
		} else if have < fn.nargs {
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
		if pv := makePartial(fn, pargs); pv.have() < len(pv.args) {
			return pv // still a partial
		}
		return env.apply(fn.fn, pargs...)

	case HoleValue:
		if len(args) != 1 {
			panic(fmt.Sprintf("hole expects a single argument, got %v", len(args)))
		}
		return args[0]

	case *GoValue:
		argStrs := make([]string, len(args))
		for i, a := range args {
			switch a := a.(type) {
			case *IntegerValue:
				argStrs[i] = a.String()
			case *StringValue:
				argStrs[i] = strconv.Quote(a.String())
			default:
				panic(fmt.Sprintf("unhandled argument type %T", a))
			}
		}
		v, err := env.gointerp.Eval(fn.name + "(" + strings.Join(argStrs, ", ") + ")")
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

func (env *Environment) apply1(fn Value, arg Value) Value {
	argsptr := argsPool.Get().(*[]Value)
	defer argsPool.Put(argsptr)
	args := *argsptr
	args[0] = arg
	return env.apply(fn, args...)
}

var argsPool = sync.Pool{
	New: func() interface{} {
		args := make([]Value, 1)
		return &args
	},
}

func New() *Environment {
	i := interp.New(interp.Options{GoPath: "/Users/luke/go"})
	i.Use(stdlib.Symbols)
	pkgs := []string{"crypto/md5", "fmt", "encoding/hex", "strconv", "strings"}
	for _, pkg := range pkgs {
		if _, err := i.Eval(fmt.Sprintf("import %q", pkg)); err != nil {
			panic(err)
		}
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
