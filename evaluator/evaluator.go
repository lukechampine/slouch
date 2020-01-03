// Package evaluator ...
package evaluator

import (
	"fmt"
	"io/ioutil"
	"strconv"
	"strings"

	"lukechampine.com/slouch/ast"
	"lukechampine.com/slouch/token"
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

type ArrayValue struct {
	elems []Value
}

func (av ArrayValue) isValue() {}
func (av ArrayValue) String() string {
	strs := make([]string, len(av.elems))
	for i := range strs {
		strs[i] = av.elems[i].String()
	}
	return "[" + strings.Join(strs, " ") + "]"
}

type LambdaValue struct {
	body ast.Expression
}

func (lv LambdaValue) isValue() {}
func (lv LambdaValue) String() string {
	return "| " + lv.body.String()
}

func equals(x, y Value) bool {
	switch x := x.(type) {
	case IntegerValue:
		yv, ok := y.(IntegerValue)
		if !ok {
			panic("invalid comparison")
		}
		return x.i == yv.i
	case StringValue:
		yv, ok := y.(StringValue)
		if !ok {
			panic("invalid comparison")
		}
		return x.s == yv.s
	case ArrayValue:
		yv, ok := y.(ArrayValue)
		if !ok {
			panic("invalid comparison")
		}
		if len(x.elems) != len(yv.elems) {
			return false
		}
		for i := range x.elems {
			if !equals(x.elems[i], yv.elems[i]) {
				return false
			}
		}
		return true
	default:
		panic("unhandled comparison")
	}
}

func Eval(n ast.Node) Value {
	return New().Eval(n)
}

type Environment struct {
	vars map[string]Value
	// TODO: may not need to be this generic; if fn args have fixed names ($1,
	// $2, etc.) then we can just store those, not an arbitrary mapping
	parent *Environment
}

func New() *Environment {
	return &Environment{
		vars: make(map[string]Value),
	}
}

func (env *Environment) lookup(name string) (Value, bool) {
	v, ok := env.vars[name]
	if !ok && env.parent != nil {
		return env.parent.lookup(name)
	}
	return v, ok
}

func (env *Environment) Eval(n ast.Expression) Value { return env.evalExpression(n) }

func prependPipe(p ast.Expression, in ast.Expression) ast.Expression {
	switch p := p.(type) {
	case ast.Pipe:
		p.Left = prependPipe(p.Left, in)
		return p
	case ast.FunctionCall:
		p.Args = append(p.Args, in)
		return p
	default:
		panic("can't prepend")
	}
}

func (env *Environment) evalFunctionCall(fc ast.FunctionCall) Value {
	// TODO: fc.Fn = evaluate(fc.Fn)? For higher-order functions, I suppose

	switch fn := fc.Fn.(type) {
	case ast.Identifier:
		v, ok := env.lookup(fn.Name)
		if !ok {
			panic("unknown function " + fn.Name)
		}
		switch v := v.(type) {
		case LambdaValue:
			var in ast.Expression = ast.Array{Elements: fc.Args}
			if len(fc.Args) == 1 {
				in = fc.Args[0]
			}
			return env.evalExpression(prependPipe(v.body, in))
		default:
			// "calling" a non-lambda simply returns the value
			if len(fc.Args) != 0 {
				panic("non-function call should not have any args")
			}
			return v
		}
	case ast.Builtin:
		return map[string]func([]ast.Expression) Value{
			"cat":   env.implCat,
			"count": env.implCount,
			"inc":   env.implInc,
			"sum":   env.implSum,
		}[fn.Name](fc.Args)
	default:
		fmt.Printf("%s %T\n", fn, fn)
		panic("can't call non-identifiers")
	}
}

func (env *Environment) implInc(args []ast.Expression) Value {
	if len(args) != 1 {
		panic("wrong number of args")
	}
	iv, ok := env.evalExpression(args[0]).(IntegerValue)
	if !ok {
		panic("not an int")
	}
	return IntegerValue{iv.i + 1}
}

func (env *Environment) implCat(args []ast.Expression) Value {
	if len(args) != 1 {
		panic("wrong number of args")
	}
	sv, ok := env.evalExpression(args[0]).(StringValue)
	if !ok {
		panic("not a string")
	}
	data, err := ioutil.ReadFile(sv.s)
	if err != nil {
		panic(err)
	}
	return StringValue{string(data)}
}

func (env *Environment) implCount(args []ast.Expression) Value {
	if len(args) != 2 {
		panic("wrong number of args")
	}
	var n int64
	cv := env.evalExpression(args[0])
	switch arr := env.evalExpression(args[1]).(type) {
	case ArrayValue:
		if len(arr.elems) == 0 {
			return IntegerValue{0}
		}
		for _, e := range arr.elems {
			if equals(e, cv) {
				n++
			}
		}
	case StringValue:
		if len(arr.s) == 0 {
			return IntegerValue{0}
		}
		sv, ok := cv.(StringValue)
		if !ok {
			panic("not a string")
		}
		n = int64(strings.Count(arr.s, sv.s))
	default:
		panic("invalid type for count")
	}
	return IntegerValue{n}
}

func (env *Environment) implSum(args []ast.Expression) Value {
	if len(args) != 1 {
		panic("wrong number of args")
	}
	av, ok := env.evalExpression(args[0]).(ArrayValue)
	if !ok {
		panic("not an array")
	}
	var sum int64
	for _, e := range av.elems {
		switch v := e.(type) {
		case IntegerValue:
			sum += v.i
		default:
			panic("can't add non-integer type")
		}
	}
	return IntegerValue{sum}
}

func (env *Environment) opPlus(l, r Value) Value {
	switch l := l.(type) {
	case IntegerValue:
		switch r := r.(type) {
		case IntegerValue:
			return IntegerValue{l.i + r.i}
		default:
			panic("illegal plus types")
		}

	case StringValue:
		switch r := r.(type) {
		case StringValue:
			return StringValue{l.s + r.s}
		default:
			panic("illegal plus types")
		}

	case ArrayValue:
		switch r := r.(type) {
		case IntegerValue:
			for i, le := range l.elems {
				l.elems[i] = env.opPlus(le, r)
			}
			return l

		case StringValue:
			for i, le := range l.elems {
				l.elems[i] = env.opPlus(le, r)
			}
			return l

		case ArrayValue:
			return ArrayValue{append(l.elems, r.elems...)}

		default:
			panic("illegal plus types")
		}

	default:
		panic("unhandled plus type")
	}
}

func (env *Environment) opMinus(l, r Value) Value {
	switch l := l.(type) {
	case IntegerValue:
		switch r := r.(type) {
		case IntegerValue:
			return IntegerValue{l.i - r.i}
		default:
			panic("illegal minus types")
		}

	case StringValue:
		switch r := r.(type) {
		case StringValue:
			var rem []rune
		strLoop:
			for _, lc := range l.s {
				for _, rc := range r.s {
					if rc == lc {
						continue strLoop
					}
				}
				rem = append(rem, lc)
			}
			return StringValue{string(rem)}
		default:
			panic("illegal minus types")
		}

	case ArrayValue:
		switch r := r.(type) {
		case IntegerValue:
			for i, le := range l.elems {
				l.elems[i] = env.opMinus(le, r)
			}
			return l

		case StringValue:
			for i, le := range l.elems {
				l.elems[i] = env.opMinus(le, r)
			}
			return l

		case ArrayValue:
			var rem []Value
		arrLoop:
			for _, le := range l.elems {
				for _, re := range r.elems {
					if equals(le, re) {
						continue arrLoop
					}
				}
				rem = append(rem, le)
			}
			return ArrayValue{rem}
		default:
			panic("illegal minus types")
		}

	default:
		panic("unhandled minus type")
	}
}

func (env *Environment) evalExpression(n ast.Node) Value {
	switch n := n.(type) {
	case ast.FunctionCall:
		return env.evalFunctionCall(n)

	case ast.Assignment:
		v := env.evalExpression(n.Value)
		env.vars[n.Var.Name] = v
		return v

	case ast.InfixOp:
		return map[token.Kind]func(l, r Value) Value{
			token.Plus:  env.opPlus,
			token.Minus: env.opMinus,
		}[n.Operator](env.evalExpression(n.Left), env.evalExpression(n.Right))

	case ast.Pipe:
		switch dst := n.Right.(type) {
		case ast.FunctionCall:
			dst.Args = append(dst.Args, n.Left)
			return env.evalFunctionCall(dst)
		default:
			// arbitrary expression; evaluate with $ set to lhs
			env.vars["x"] = env.evalExpression(n.Left)
			return env.evalExpression(n.Right)
		}

	case ast.Integer:
		i, _ := strconv.ParseInt(n.Value, 0, 64) // TODO: parse as bigint
		return IntegerValue{i}

	case ast.String:
		return StringValue{n.Value}

	case ast.Array:
		elems := make([]Value, len(n.Elements))
		for i := range elems {
			elems[i] = env.evalExpression(n.Elements[i])
		}
		return ArrayValue{elems}

	case ast.Identifier:
		v, ok := env.lookup(n.Name)
		if !ok {
			panic("no variable found")
		}
		return v

	case ast.Lambda:
		return LambdaValue{n.Body}

	default:
		panic(fmt.Sprintf("couldn't evaluate type %T", n))
	}
}
