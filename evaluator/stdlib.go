package evaluator

import (
	"fmt"
	"reflect"
	"strconv"
	"strings"

	"lukechampine.com/slouch/token"
)

var infixFns = map[token.Kind]BuiltinValue{
	token.Plus:   makeBuiltin("+", builtinAdd),
	token.Star:   makeBuiltin("*", builtinMultiply),
	token.Equals: makeBuiltin("==", builtinEquals),
}

var builtins = [...]BuiltinValue{
	makeBuiltin("add", builtinAdd),
	makeBuiltin("choose", builtinChoose),
	makeBuiltin("first", builtinFirst),
	makeBuiltin("head", builtinHead),
	makeBuiltin("lines", builtinLines),
	makeBuiltin("ints", builtinInts),
	makeBuiltin("map", builtinMap),
	makeBuiltin("reverse", builtinReverse),
	makeBuiltin("sum", builtinSum),
	makeBuiltin("product", builtinProduct),
	makeBuiltin("take", builtinTake),
}

func makeBuiltin(name string, fn interface{}) BuiltinValue {
	v, t := reflect.ValueOf(fn), reflect.TypeOf(fn)
	if t.Kind() != reflect.Func || t.NumOut() != 1 {
		panic("wrapped function has wrong type signature")
	}
	takesEnv := t.In(0) == reflect.TypeOf(new(Environment))
	nargs := t.NumIn()
	if takesEnv {
		nargs--
	}
	return BuiltinValue{
		name:  name,
		nargs: nargs,
		fn: func(env *Environment, args []Value) Value {
			argVals := make([]reflect.Value, 0, t.NumIn())
			if takesEnv {
				argVals = append(argVals, reflect.ValueOf(env))
			}
			for _, a := range args {
				argVals = append(argVals, reflect.ValueOf(a))
			}
			for i := range argVals {
				if !argVals[i].Type().AssignableTo(t.In(i)) {
					panic(fmt.Sprintf("%v expected %v, got %v", name, t.In(i), argVals[i].Type()))
				}
			}
			return v.Call(argVals)[0].Interface().(Value)
		},
	}
}

func internalTruthy(v Value) bool {
	switch v := v.(type) {
	case IntegerValue:
		return v.i != 0
	case StringValue:
		return v.s != ""
	case BoolValue:
		return v.b
	case ArrayValue:
		return len(v.elems) != 0
	case PartialValue:
		return true
	default:
		panic(fmt.Sprintf("truthy: unhandled type %T", v))
	}
}

func builtinAdd(l, r Value) Value {
	switch l := l.(type) {
	case IntegerValue:
		switch r := r.(type) {
		case IntegerValue:
			return IntegerValue{i: l.i + r.i}
		}
	case StringValue:
		switch r := r.(type) {
		case StringValue:
			return StringValue{s: l.s + r.s}
		}
	}
	panic(fmt.Sprintf("unhandled type combination %T + %T", l, r))
}

func builtinMultiply(l, r Value) Value {
	switch l := l.(type) {
	case IntegerValue:
		switch r := r.(type) {
		case IntegerValue:
			return IntegerValue{i: l.i * r.i}
		}
	case StringValue:
		switch r := r.(type) {
		case IntegerValue:
			return StringValue{s: strings.Repeat(l.s, int(r.i))}
		}
	}
	panic(fmt.Sprintf("unhandled type combination %T + %T", l, r))
}

func builtinEquals(l, r Value) BoolValue {
	switch l := l.(type) {
	case IntegerValue:
		switch r := r.(type) {
		case IntegerValue:
			return BoolValue{b: l.i == r.i}
		}
	case StringValue:
		switch r := r.(type) {
		case StringValue:
			return BoolValue{b: l.s == r.s}
		}
	}
	panic(fmt.Sprintf("unhandled type combination %T + %T", l, r))
}

func builtinLines(s StringValue) ArrayValue {
	lines := strings.Split(strings.TrimSpace(s.s), "\n")
	a := ArrayValue{elems: make([]Value, len(lines))}
	for i := range a.elems {
		a.elems[i] = StringValue{lines[i]}
	}
	return a
}

func builtinInts(s StringValue) ArrayValue {
	fs := strings.FieldsFunc(s.s, func(r rune) bool {
		return !('0' <= r && r <= '9') && r != '-' // 0-9 or -
	})
	a := ArrayValue{elems: make([]Value, 0, len(fs))}
	for _, w := range fs {
		i, err := strconv.Atoi(w)
		if err == nil {
			a.elems = append(a.elems, IntegerValue{i: int64(i)})
		}
	}
	return a
}

func builtinHead(a ArrayValue) Value {
	if len(a.elems) == 0 {
		panic("head called on empty array")
	}
	return a.elems[0]
}

func builtinTake(n IntegerValue, a ArrayValue) ArrayValue {
	if int(n.i) > len(a.elems) {
		n.i = int64(len(a.elems))
	}
	return ArrayValue{
		elems: a.elems[:n.i],
	}
}

func builtinChoose(n IntegerValue, a ArrayValue) ArrayValue {
	var choose func(xs []Value, n int64) [][]Value
	choose = func(xs []Value, n int64) [][]Value {
		if n == 0 {
			return [][]Value{nil}
		}
		var choices [][]Value
		for i := 0; i < len(xs); i++ {
			for _, tail := range choose(xs[i+1:], n-1) {
				choices = append(choices, append(tail, xs[i]))
			}
		}
		return choices
	}

	choices := choose(a.elems, n.i)
	cs := ArrayValue{elems: make([]Value, len(choices))}
	for i := range cs.elems {
		cs.elems[i] = builtinReverse(ArrayValue{elems: choices[i]})
	}
	return cs
}

func builtinReverse(a ArrayValue) ArrayValue {
	rev := make([]Value, len(a.elems))
	for i := range rev {
		rev[i] = a.elems[len(a.elems)-i-1]
	}
	return ArrayValue{elems: rev}
}

func builtinMap(env *Environment, fn Value, a ArrayValue) ArrayValue {
	m := make([]Value, len(a.elems))
	for i, e := range a.elems {
		m[i] = env.apply(fn, []Value{e})
	}
	return ArrayValue{elems: m}
}

func builtinSum(a ArrayValue) Value {
	if len(a.elems) == 0 {
		return IntegerValue{i: 0}
	}
	sum := a.elems[0]
	for _, e := range a.elems[1:] {
		sum = builtinAdd(sum, e)
	}
	return sum
}

func builtinProduct(a ArrayValue) Value {
	if len(a.elems) == 0 {
		return IntegerValue{i: 0}
	}
	prod := a.elems[0]
	for _, e := range a.elems[1:] {
		prod = builtinMultiply(prod, e)
	}
	return prod
}

func builtinFirst(env *Environment, fn Value, a ArrayValue) Value {
	for _, e := range a.elems {
		if internalTruthy(env.apply(fn, []Value{e})) {
			return e
		}
	}
	panic("first: no elements satisfy predicate")
}
