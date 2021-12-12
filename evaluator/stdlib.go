package evaluator

import (
	"fmt"
	"reflect"
	"regexp"
	"sort"
	"strconv"
	"strings"

	"lukechampine.com/slouch/token"
)

var infixFns = map[token.Kind]*BuiltinValue{
	token.Plus:          makeBuiltin("+", builtinAdd),
	token.Minus:         makeBuiltin("-", builtinSub),
	token.Star:          makeBuiltin("*", builtinMultiply),
	token.Slash:         makeBuiltin("/", builtinDivide),
	token.Mod:           makeBuiltin("%", builtinMod),
	token.Equals:        makeBuiltin("==", builtinEquals),
	token.NotEquals:     makeBuiltin("!=", builtinNotEquals),
	token.Less:          makeBuiltin("<", builtinLess),
	token.Greater:       makeBuiltin(">", builtinGreater),
	token.LessEquals:    makeBuiltin("<=", builtinLessEquals),
	token.GreaterEquals: makeBuiltin(">=", builtinGreaterEquals),
	token.DivisibleBy:   makeBuiltin("%?", builtinDivisibleBy),
	token.And:           makeBuiltin("and", builtinAnd),
	token.Or:            makeBuiltin("or", builtinOr),
	token.Dot:           makeBuiltin(".", builtinDot),
}

var builtins = [...]*BuiltinValue{
	makeBuiltin("abs", builtinAbs),
	makeBuiltin("adj", builtinAdj),
	makeBuiltin("adj8", builtinAdj8),
	makeBuiltin("all", builtinAll),
	makeBuiltin("alpha", builtinAlpha),
	makeBuiltin("any", builtinAny),
	makeBuiltin("append", builtinAppend),
	makeBuiltin("apply", builtinApply),
	makeBuiltin("assoc", builtinAssoc),
	makeBuiltin("caesar", builtinCaesar),
	makeBuiltin("chars", builtinChars),
	makeBuiltin("choose", builtinChoose),
	makeBuiltin("collect", builtinCollect),
	makeBuiltin("concat", builtinConcat),
	makeBuiltin("contains", builtinContains),
	makeBuiltin("containsAny", builtinContainsAny),
	makeBuiltin("dims", builtinDims),
	makeBuiltin("dfs", builtinDFS),
	makeBuiltin("flood", builtinFlood),
	makeBuiltin("exhaust", builtinExhaust),
	makeBuiltin("in", builtinIn),
	makeBuiltin("count", builtinCount),
	makeBuiltin("cycle", builtinCycle),
	makeBuiltin("delete", builtinDelete),
	makeBuiltin("deleteAll", builtinDeleteAll),
	makeBuiltin("deltas", builtinDeltas),
	makeBuiltin("diff", builtinDiff),
	makeBuiltin("digits", builtinDigits),
	makeBuiltin("drop", builtinDrop),
	makeBuiltin("dropWhile", builtinDropWhile),
	makeBuiltin("dups", builtinDups),
	makeBuiltin("enum", builtinEnum),
	makeBuiltin("fromDigits", builtinFromDigits),
	makeBuiltin("fromBase", builtinFromBase),
	makeBuiltin("toBase", builtinToBase),
	makeBuiltin("filter", builtinFilter),
	makeBuiltin("first", builtinFirst),
	makeBuiltin("firstRepeat", builtinFirstRepeat),
	makeBuiltin("flatten", builtinFlatten),
	makeBuiltin("fold", builtinFold),
	makeBuiltin("fold1", builtinFold1),
	makeBuiltin("graph", builtinGraph),
	makeBuiltin("hasKey", builtinHasKey),
	makeBuiltin("hasPrefix", builtinHasPrefix),
	makeBuiltin("hasVal", builtinHasVal),
	makeBuiltin("head", builtinHead),
	makeBuiltin("histogram", builtinHistogram),
	makeBuiltin("index", builtinIndex),
	makeBuiltin("indexIn", builtinIndexIn),
	makeBuiltin("inits", builtinInits),
	makeBuiltin("int", builtinInt),
	makeBuiltin("ints", builtinInts),
	makeBuiltin("invert", builtinInvert),
	makeBuiltin("iota", builtinIota),
	makeBuiltin("iterate", builtinIterate),
	makeBuiltin("join", builtinJoin),
	makeBuiltin("keys", builtinKeys),
	makeBuiltin("last", builtinLast),
	makeBuiltin("len", builtinLen),
	makeBuiltin("lines", builtinLines),
	makeBuiltin("map", builtinMap),
	makeBuiltin("concatmap", builtinConcatmap),
	makeBuiltin("max", builtinMax),
	makeBuiltin("maxBy", builtinMaxBy),
	makeBuiltin("maxIndex", builtinMaxIndex),
	makeBuiltin("memo", builtinMemo),
	makeBuiltin("move", builtinMove),
	makeBuiltin("draw", builtinDraw),
	makeBuiltin("min", builtinMin),
	makeBuiltin("minBy", builtinMinBy),
	makeBuiltin("minIndex", builtinMinIndex),
	makeBuiltin("mean", builtinMean),
	makeBuiltin("median", builtinMedian),
	makeBuiltin("none", builtinNone),
	makeBuiltin("not", builtinNot),
	makeBuiltin("partition", builtinPartition),
	makeBuiltin("perms", builtinPerms),
	makeBuiltin("prepend", builtinPrepend),
	makeBuiltin("product", builtinProduct),
	makeBuiltin("regex", builtinRegex),
	makeBuiltin("repeat", builtinRepeat),
	makeBuiltin("reverse", builtinReverse),
	makeBuiltin("rotate", builtinRotate),
	makeBuiltin("runs", builtinRuns),
	makeBuiltin("same", builtinSame),
	makeBuiltin("scan", builtinScan),
	makeBuiltin("set", builtinSet),
	makeBuiltin("sort", builtinSort),
	makeBuiltin("sortBy", builtinSortBy),
	makeBuiltin("sorted", builtinSorted),
	makeBuiltin("sortedBy", builtinSortedBy),
	makeBuiltin("split", builtinSplit),
	makeBuiltin("stabilize", builtinStabilize),
	makeBuiltin("string", builtinString),
	makeBuiltin("sum", builtinSum),
	makeBuiltin("tail", builtinTail),
	makeBuiltin("tails", builtinTails),
	makeBuiltin("take", builtinTake),
	makeBuiltin("takeWhile", builtinTakeWhile),
	makeBuiltin("toUpper", builtinToUpper),
	makeBuiltin("transpose", builtinTranspose),
	makeBuiltin("uniq", builtinUniq),
	makeBuiltin("uniqBy", builtinUniqBy),
	makeBuiltin("vals", builtinVals),
	makeBuiltin("window", builtinWindow),
	makeBuiltin("within", builtinWithin),
	makeBuiltin("words", builtinWords),
	makeBuiltin("zip", builtinZip),
	makeBuiltin("zipWith", builtinZipWith),
}

var stringTyp = reflect.TypeOf(&StringValue{})
var arrayTyp = reflect.TypeOf(&ArrayValue{})
var mapTyp = reflect.TypeOf(&MapValue{})
var iteratorTyp = reflect.TypeOf(&IteratorValue{})

func makeBuiltin(name string, fn interface{}) *BuiltinValue {
	v, t := reflect.ValueOf(fn), reflect.TypeOf(fn)
	if t.Kind() != reflect.Func || t.NumOut() != 1 {
		panic("wrapped function has wrong type signature")
	}
	nargs := t.NumIn()
	takesEnv := nargs > 0 && t.In(0) == reflect.TypeOf(new(Environment))
	if takesEnv {
		nargs--
	}
	argVals := make([]reflect.Value, t.NumIn())
	return &BuiltinValue{
		name:  name,
		nargs: nargs,
		fn: func(env *Environment, args []Value) Value {
			argVals = argVals[:0]
			if takesEnv {
				argVals = append(argVals, reflect.ValueOf(env))
			}
			for _, a := range args {
				argVals = append(argVals, reflect.ValueOf(a))
			}
			for i, arg := range argVals {
				if t.In(i) == iteratorTyp && arg.Type() != iteratorTyp {
					// convert string/array/map to iterator
					var it *IteratorValue
					switch arg := arg.Interface().(type) {
					case *StringValue:
						it = internalStringIterator(arg)
					case *ArrayValue:
						it = internalArrayIterator(arg)
					case *MapValue:
						it = internalMapIterator(arg)
					default:
						panic(fmt.Sprintf("%v expected %v, got %v", name, t.In(i), arg))
					}
					argVals[i] = reflect.ValueOf(it)
				} else if arg.Type() == iteratorTyp && t.In(i) == arrayTyp {
					argVals[i] = reflect.ValueOf(builtinCollect(arg.Interface().(*IteratorValue)))
				} else if arg.Type() == iteratorTyp && t.In(i) == stringTyp {
					argVals[i] = reflect.ValueOf(builtinConcat(arg.Interface().(*IteratorValue)))
				} else if arg.Type() == arrayTyp && t.In(i) == stringTyp {
					argVals[i] = reflect.ValueOf(builtinConcat(internalArrayIterator(arg.Interface().(*ArrayValue))))
				} else if !arg.Type().AssignableTo(t.In(i)) {
					panic(fmt.Sprintf("%v expected %v, got %v", name, t.In(i), arg.Type()))
				}
			}
			return v.Call(argVals)[0].Interface().(Value)
		},
	}
}

func internalToIterator(v Value) *IteratorValue {
	switch v := v.(type) {
	case *IteratorValue:
		return v
	case *StringValue:
		return internalStringIterator(v)
	case *ArrayValue:
		return internalArrayIterator(v)
	case *MapValue:
		return internalMapIterator(v)
	default:
		return &IteratorValue{
			next: func() Value {
				r := v
				v = nil
				return r
			},
		}
	}
}

func internalTruthy(v Value) bool {
	switch v := v.(type) {
	case *IntegerValue:
		return v.i != 0
	case *StringValue:
		return v.s != ""
	case *BoolValue:
		return v.b
	case *ArrayValue:
		return len(v.elems) != 0
	default:
		panic(fmt.Sprintf("truthy: unhandled type %T", v))
	}
}

func internalStringIterator(s *StringValue) *IteratorValue {
	i := -1
	return &IteratorValue{
		next: func() Value {
			if i++; i >= len(s.s) {
				return nil
			}
			return makeString(s.s[i:][:1])
		},
	}
}

func internalArrayIterator(a *ArrayValue) *IteratorValue {
	i := -1
	return &IteratorValue{
		next: func() Value {
			if i++; i >= len(a.elems) {
				return nil
			}
			return a.elems[i]
		},
	}
}

func internalMapIterator(m *MapValue) *IteratorValue {
	i := -1
	return &IteratorValue{
		next: func() Value {
			if i++; i >= len(m.keys) {
				return nil
			}
			return makeArray([]Value{m.keys[i], m.vals[i]})
		},
	}
}

func internalLess(a, b Value) bool {
	switch a := a.(type) {
	case *IntegerValue:
		if b, ok := b.(*IntegerValue); ok {
			return a.i < b.i
		}
	case *StringValue:
		if b, ok := b.(*StringValue); ok {
			return a.s < b.s
		}
	}
	panic(fmt.Sprintf("cannot order %T relative to %T", a, b))
}

func internalNegate(v Value) Value {
	switch v := v.(type) {
	case *IntegerValue:
		return &IntegerValue{-v.i}
	case *BoolValue:
		return &BoolValue{!v.b}
	}
	panic(fmt.Sprintf("cannot negate %T", v))
}

func internalEquals(l, r Value) bool {
	if a, ok := l.(*ArrayValue); ok {
		l = internalArrayIterator(a)
	}
	if a, ok := r.(*ArrayValue); ok {
		r = internalArrayIterator(a)
	}

	switch l := l.(type) {
	case *IntegerValue:
		switch r := r.(type) {
		case *IntegerValue:
			return l.i == r.i
		}
	case *BoolValue:
		switch r := r.(type) {
		case *BoolValue:
			return l.b == r.b
		}
	case *StringValue:
		switch r := r.(type) {
		case *StringValue:
			return l.s == r.s
		case *IteratorValue:
			return l.s == builtinConcat(r).(*StringValue).s
		}
	case *IteratorValue:
		switch r := r.(type) {
		case *StringValue:
			return internalEquals(l, internalStringIterator(r))
		case *IteratorValue:
			if l.infinite != r.infinite {
				return false
			} else if l.infinite && r.infinite {
				panic("cannot compare infinite iterators")
			}
			for {
				lv, rv := l.next(), r.next()
				if lv == nil && rv == nil {
					return true
				} else if lv == nil || rv == nil {
					return false
				} else if !internalEquals(lv, rv) {
					return false
				}
			}
		}
	}
	panic(fmt.Sprintf("unhandled type combination %T == %T", l, r))
}

func internalClone(v Value) Value {
	switch v := v.(type) {
	case *IteratorValue:
		a := builtinCollect(v)
		for i := range a.elems {
			a.elems[i] = internalClone(a.elems[i])
		}
		return a
	case *ArrayValue:
		a := makeArray(append([]Value(nil), v.elems...))
		for i := range a.elems {
			a.elems[i] = internalClone(a.elems[i])
		}
		return a
	case *MapValue:
		m := &MapValue{
			keys:    append([]Value(nil), v.keys...),
			vals:    append([]Value(nil), v.vals...),
			indices: make(map[valueHash]int),
		}
		for i := range m.keys {
			m.keys[i] = internalClone(m.keys[i])
		}
		for i := range m.vals {
			m.vals[i] = internalClone(m.vals[i])
		}
		for h, i := range v.indices {
			m.indices[h] = i
		}
		return m
	default:
		return v
	}
}

func builtinCollect(it *IteratorValue) *ArrayValue {
	if it.infinite {
		panic("can't collect infinite iterator")
	}
	var elems []Value
	for e := it.next(); e != nil; e = it.next() {
		if eit, ok := e.(*IteratorValue); ok {
			e = builtinCollect(eit)
		}
		elems = append(elems, e)
	}
	return makeArray(elems)
}

func builtinAdd(l, r Value) Value {
	switch l := l.(type) {
	case *IntegerValue:
		switch r := r.(type) {
		case *IntegerValue:
			return makeInteger(l.i + r.i)
		}
	case *StringValue:
		switch r := r.(type) {
		case *StringValue:
			return makeString(l.s + r.s)
		}
	}
	panic(fmt.Sprintf("unhandled type combination %T + %T", l, r))
}

func builtinSub(l, r Value) Value {
	switch l := l.(type) {
	case *IntegerValue:
		switch r := r.(type) {
		case *IntegerValue:
			return makeInteger(l.i - r.i)
		}
	}
	panic(fmt.Sprintf("unhandled type combination %T - %T", l, r))
}

func builtinMultiply(l, r Value) Value {
	switch l := l.(type) {
	case *IntegerValue:
		switch r := r.(type) {
		case *IntegerValue:
			return makeInteger(l.i * r.i)
		}
	case *StringValue:
		switch r := r.(type) {
		case *IntegerValue:
			return makeString(strings.Repeat(l.s, int(r.i)))
		}
	}
	panic(fmt.Sprintf("unhandled type combination %T * %T", l, r))
}

func builtinDivide(l, r Value) Value {
	switch l := l.(type) {
	case *IntegerValue:
		switch r := r.(type) {
		case *IntegerValue:
			return makeInteger(l.i / r.i)
		}
	}
	panic(fmt.Sprintf("unhandled type combination %T / %T", l, r))
}

func builtinMod(l, r Value) Value {
	switch l := l.(type) {
	case *IntegerValue:
		switch r := r.(type) {
		case *IntegerValue:
			return makeInteger(l.i % r.i)
		}
	}
	panic(fmt.Sprintf("unhandled type combination %T % %T", l, r))
}

func builtinEquals(l, r Value) *BoolValue {
	return makeBool(internalEquals(l, r))
}

func builtinNotEquals(l, r Value) *BoolValue {
	return makeBool(!internalEquals(l, r))
}

func builtinLess(l, r Value) *BoolValue {
	return makeBool(internalLess(l, r))
}

func builtinGreater(l, r Value) *BoolValue {
	return makeBool(internalLess(r, l))
}

func builtinLessEquals(l, r Value) *BoolValue {
	return makeBool(internalLess(l, r) || internalEquals(l, r))
}

func builtinGreaterEquals(l, r Value) *BoolValue {
	return makeBool(internalLess(r, l) || internalEquals(l, r))
}

func builtinDivisibleBy(l, r Value) *BoolValue {
	return builtinEquals(builtinMod(l, r), makeInteger(0))
}

func builtinAnd(l, r Value) *BoolValue {
	return makeBool(internalTruthy(l) && internalTruthy(r))
}

func builtinOr(l, r Value) *BoolValue {
	return makeBool(internalTruthy(l) || internalTruthy(r))
}

func builtinNot(env *Environment, b Value) Value {
	switch b := b.(type) {
	case *BoolValue:
		return makeBool(!b.b)
	case *PartialValue:
		return env.apply(&BuiltinValue{
			name:  "notpartial",
			nargs: env.apply(b).(*PartialValue).need(),
			fn: func(env *Environment, args []Value) Value {
				return makeBool(!env.apply(b, args...).(*BoolValue).b)
			},
		})
	default:
		panic(fmt.Sprintf("not: invalid type %T", b))
	}
}

func builtinDot(l, r Value) Value {
	if it, ok := r.(*IteratorValue); ok {
		r = builtinCollect(it)
	}
	if r, ok := r.(*ArrayValue); ok {
		e := builtinDot(l, r.elems[len(r.elems)-1])
		if r := makeArray(r.elems[:len(r.elems)-1]); len(r.elems) > 0 {
			return builtinDot(e, r)
		}
		return e
	}

	switch l := l.(type) {
	case *ArrayValue:
		switch r := r.(type) {
		case *IntegerValue:
			return l.elems[r.i]
		}
	case *StringValue:
		switch r := r.(type) {
		case *IntegerValue:
			return makeString(l.s[r.i:][:1])
		}
	case *MapValue:
		v := l.get(r)
		if v == nil {
			// TODO: maybe return false?
			panic(fmt.Sprintf("map does not contain %v", r))
		}
		return v
	case *IteratorValue:
		switch r := r.(type) {
		case *IntegerValue:
			for n := r.i; n > 0; n-- {
				l.next()
			}
			v := l.next()
			if v == nil {
				// TODO: maybe return false?
				panic("exhausted iterator")
			}
		}
	}
	panic(fmt.Sprintf("unhandled type combination %T . %T", l, r))
}

func builtinSplit(split Value, it Value) Value {
	if a, ok := it.(*ArrayValue); ok {
		it = internalArrayIterator(a)
	}

	switch it := it.(type) {
	case *StringValue:
		groups := strings.Split(strings.TrimSpace(it.s), split.(*StringValue).s)
		elems := make([]Value, len(groups))
		for i := range elems {
			elems[i] = makeString(groups[i])
		}
		return makeArray(elems)

	case *IteratorValue:
		var a []Value
		return &IteratorValue{
			next: func() Value {
			again:
				v := it.next()
				if v == nil && a == nil {
					return nil
				}
				if v == nil || internalEquals(v, split) {
					v = makeArray(a)
					a = nil
					return v
				}
				a = append(a, v)
				goto again
			},
			infinite: it.infinite,
		}
	default:
		panic(fmt.Sprintf("split: invalid type %T", it))
	}
}

func builtinJoin(between Value, it *IteratorValue) *IteratorValue {
	joining := true
	return &IteratorValue{
		next: func() Value {
			joining = !joining
			if joining {
				return between
			}
			return it.next()
		},
	}
}

func builtinLines(s *StringValue) Value {
	return builtinSplit(makeString("\n"), s)
}

func builtinWords(s *StringValue) *ArrayValue {
	words := strings.Fields(strings.TrimSpace(s.s))
	elems := make([]Value, len(words))
	for i := range elems {
		elems[i] = makeString(words[i])
	}
	return makeArray(elems)
}

func builtinChars(s *StringValue) *ArrayValue {
	elems := make([]Value, len(s.s))
	for i := range elems {
		elems[i] = makeString(s.s[i:][:1])
	}
	return makeArray(elems)
}

func builtinDigits(v Value) *ArrayValue {
	if a, ok := v.(*ArrayValue); ok {
		v = internalArrayIterator(a)
	}
	if it, ok := v.(*IteratorValue); ok {
		v = builtinConcat(it)
	}
	switch v := v.(type) {
	case *IntegerValue:
		var elems []Value
		for v.i != 0 {
			elems = append(elems, makeInteger(v.i%10))
			v.i /= 10
		}
		for i := 0; i < len(elems)/2; i++ {
			j := len(elems) - i - 1
			elems[i], elems[j] = elems[j], elems[i]
		}
		return makeArray(elems)
	case *StringValue:
		elems := make([]Value, len(v.s))
		for i, c := range v.s {
			d := int64(c - '0')
			if d < 0 || d > 9 {
				panic("invalid digit")
			}
			elems[i] = makeInteger(d)
		}
		return makeArray(elems)
	default:
		panic(fmt.Sprintf("digits: unhandled type %T", v))
	}
}

func builtinInt(v Value) *IntegerValue {
	if it, ok := v.(*IteratorValue); ok {
		v = builtinConcat(it)
	}
	switch v := v.(type) {
	case *StringValue:
		i, err := strconv.Atoi(v.s)
		if err != nil {
			panic(err)
		}
		return makeInteger(int64(i))
	case *BoolValue:
		if !v.b {
			return makeInteger(0)
		}
		return makeInteger(1)
	default:
		panic(fmt.Sprintf("int: unhandled type %T", v))
	}
}

func builtinInts(s *StringValue) *ArrayValue {
	fs := strings.FieldsFunc(s.s, func(r rune) bool {
		return !('0' <= r && r <= '9') && r != '-' // 0-9 or -
	})
	elems := make([]Value, 0, len(fs))
	for _, w := range fs {
		i, err := strconv.Atoi(w)
		if err == nil {
			elems = append(elems, makeInteger(int64(i)))
		}
	}
	return makeArray(elems)
}

func builtinRegex(re *StringValue, s *StringValue) *ArrayValue {
	r := regexp.MustCompile(re.s)
	matches := r.FindStringSubmatch(s.s)[1:]
	elems := make([]Value, len(matches))
	for i := range elems {
		elems[i] = makeString(matches[i])
	}
	return makeArray(elems)
}

func builtinString(v Value) *StringValue {
	return makeString(v.String())
}

func builtinHead(it *IteratorValue) Value {
	v := it.next()
	if v == nil {
		panic("head called on empty array or iterator")
	}
	return v
}

func builtinLast(it *IteratorValue) Value {
	v := it.next()
	if v == nil {
		panic("last called on empty array or iterator")
	}
	for next := it.next(); next != nil; next = it.next() {
		v = next
	}
	return v
}

func builtinTail(it *IteratorValue) Value {
	v := it.next()
	if v == nil {
		panic("tail called on empty array or iterator")
	}
	return it
}

func builtinTails(it *IteratorValue) Value {
	done := false
	a := builtinCollect(it)
	return &IteratorValue{
		next: func() Value {
			if done {
				return nil
			}
			v := makeArray(a.elems)
			if len(a.elems) == 0 {
				done = true
			} else {
				a.elems = a.elems[1:]
			}
			return v
		},
	}
}

func builtinIndex(c Value, it *IteratorValue) Value {
	var i int64
	for v := it.next(); v != nil; v = it.next() {
		if internalEquals(c, v) {
			return makeInteger(i)
		}
		i++
	}
	panic("index: no matches")
}

func builtinIndexIn(it *IteratorValue, c Value) Value {
	return builtinIndex(c, it)
}

func builtinInits(it *IteratorValue) Value {
	a := builtinCollect(it)
	i := 0
	return &IteratorValue{
		next: func() Value {
			if i > len(a.elems) {
				return nil
			}
			v := makeArray(a.elems[:i])
			i++
			return v
		},
	}
}

func builtinToUpper(s *StringValue) *StringValue {
	return makeString(strings.ToUpper(s.s))
}

func builtinTake(n *IntegerValue, it *IteratorValue) *IteratorValue {
	rem := n.i
	return &IteratorValue{
		next: func() Value {
			if rem <= 0 {
				return nil
			}
			rem--
			return it.next()
		},
	}
}

func builtinDrop(n *IntegerValue, it *IteratorValue) *IteratorValue {
	for rem := n.i; rem > 0; rem-- {
		it.next()
	}
	return it
}

func builtinTakeWhile(env *Environment, fn Value, it *IteratorValue) *IteratorValue {
	done := false
	return &IteratorValue{
		next: func() Value {
			v := it.next()
			if done || v == nil {
				return nil
			}
			c := internalClone(v)
			if !internalTruthy(env.apply1(fn, c)) {
				done = true
				return nil
			}
			return v
		},
	}
}

func builtinDropWhile(env *Environment, fn Value, it *IteratorValue) *IteratorValue {
	var fst Value
	for fst = it.next(); fst != nil; fst = it.next() {
		c := internalClone(fst)
		if internalTruthy(env.apply1(fn, c)) {
			break
		}
	}
	return &IteratorValue{
		next: func() Value {
			if fst != nil {
				v := fst
				fst = nil
				return v
			}
			return it.next()
		},
	}
}

func builtinFilter(env *Environment, fn Value, it *IteratorValue) *IteratorValue {
	return &IteratorValue{
		next: func() Value {
		again:
			v := it.next()
			if v == nil {
				return nil
			}
			c := internalClone(v)
			if !internalTruthy(env.apply1(fn, c)) {
				goto again
			}
			return v
		},
	}
}

func builtinRuns(it *IteratorValue) *IteratorValue {
	prev := it.next()
	return &IteratorValue{
		next: func() Value {
			if prev == nil {
				return nil
			}
			var g []Value
			for {
				g = append(g, prev)
				prev = it.next()
				if prev == nil || !internalEquals(prev, g[len(g)-1]) {
					break
				}
			}
			return makeArray(g)
		},
	}
}

func builtinWindow(n *IntegerValue, it Value) *IteratorValue {
	switch it := it.(type) {
	case *StringValue:
		var i int64
		return &IteratorValue{
			next: func() Value {
				if len(it.s[i:]) < int(n.i) {
					return nil
				}
				i++
				return makeString(it.s[i-1:][:n.i])
			},
		}

	case *ArrayValue:
		elems := make([]Value, n.i)
		i := copy(elems[1:], it.elems)
		return &IteratorValue{
			next: func() Value {
				if i == len(it.elems) {
					return nil
				}
				elems = append(append([]Value(nil), elems[1:]...), it.elems[i])
				i++
				return makeArray(elems)
			},
		}

	case *IteratorValue:
		elems := make([]Value, 1, n.i)
		for range elems[:n.i-1] {
			if v := it.next(); v != nil {
				elems = append(elems, v)
			}
		}
		return &IteratorValue{
			next: func() Value {
				v := it.next()
				if v == nil {
					return nil
				}
				elems = append(append([]Value(nil), elems[1:]...), v)
				return makeArray(elems)
			},
		}
	}
	panic(fmt.Sprintf("count: unhandled type %T", it))
}

func builtinPartition(n *IntegerValue, it Value) Value {
	switch it := it.(type) {
	case *StringValue:
		var parts []Value
		s := it.s
		for len(s) > int(n.i) {
			parts = append(parts, makeString(s[:n.i]))
			s = s[n.i:]
		}
		if len(s) > 0 {
			parts = append(parts, makeString(s))
		}
		return makeArray(parts)
	case *ArrayValue:
		var parts []Value
		elems := append([]Value(nil), it.elems...)
		for len(elems) > int(n.i) {
			parts = append(parts, makeArray(elems[:n.i]))
			elems = elems[n.i:]
		}
		if len(elems) > 0 {
			parts = append(parts, makeArray(elems))
		}
		return makeArray(parts)
	case *IteratorValue:
		return &IteratorValue{
			next: func() Value {
				a := builtinCollect(builtinTake(n, it))
				if len(a.elems) == 0 {
					return nil
				}
				return a
			},
		}
	default:
		panic(fmt.Sprintf("cannot partition %T", it))
	}
}

func builtinChoose(n *IntegerValue, a *ArrayValue) *IteratorValue {
	if n.i > int64(len(a.elems)) {
		panic("cannot choose more elements than array length")
	}
	choices := make([]int, n.i)
	for i := range choices {
		choices[i] = i
	}
	choices[len(choices)-1]--
	next := func() Value {
		for i := len(choices) - 1; i >= 0; i-- {
			choices[i]++
			if choices[i] < len(a.elems)-len(choices[i+1:]) {
				for i++; i < len(choices); i++ {
					choices[i] = choices[i-1] + 1
				}
				break
			} else if i == 0 {
				return nil
			}
		}
		elems := make([]Value, n.i)
		for i, c := range choices {
			elems[i] = a.elems[c]
		}
		return makeArray(elems)
	}
	return &IteratorValue{next: next}
}

func builtinPerms(a *ArrayValue) *IteratorValue {
	perm := make([]int, len(a.elems))
	for i := range perm {
		perm[i] = i
	}
	done := false
	next := func() Value {
		if done {
			return nil
		}

		elems := make([]Value, len(a.elems))
		for i, p := range perm {
			elems[i] = a.elems[p]
		}
		a := makeArray(elems)

		// if the array is not completely reversed, permute
		i := len(perm) - 2
		for i >= 0 && perm[i] > perm[i+1] {
			i--
		}
		done = i < 0
		if !done {
			// swap i with the first element greater than it
			j := len(perm) - 1
			for perm[i] > perm[j] {
				j--
			}
			perm[i], perm[j] = perm[j], perm[i]

			// reverse perm[i+1:] in place
			tail := perm[i+1:]
			for si := 0; si < len(tail)/2; si++ {
				sj := len(tail) - si - 1
				tail[si], tail[sj] = tail[sj], tail[si]
			}
		}

		return a
	}
	return &IteratorValue{next: next}
}

func builtinReverse(v Value) Value {
	if it, ok := v.(*IteratorValue); ok {
		v = builtinCollect(it)
	}
	switch v := v.(type) {
	case *ArrayValue:
		rev := make([]Value, len(v.elems))
		for i := range rev {
			rev[i] = v.elems[len(v.elems)-i-1]
		}
		return makeArray(rev)
	case *StringValue:
		rev := make([]byte, len(v.s))
		for i := range rev {
			rev[i] = v.s[len(v.s)-i-1]
		}
		return makeString(string(rev))
	default:
		panic(fmt.Sprintf("cannot reverse %T", v))
	}
}

func builtinTranspose(a *ArrayValue) *ArrayValue {
	if len(a.elems) == 0 {
		return a
	}
	switch fst := a.elems[0].(type) {
	case *ArrayValue:
		ta := make([]Value, len(fst.elems))
		for i := range ta {
			av := make([]Value, len(a.elems))
			for j := range av {
				av[j] = a.elems[j].(*ArrayValue).elems[i]
			}
			ta[i] = makeArray(av)
		}
		return makeArray(ta)
	case *StringValue:
		ta := make([]Value, len(fst.s))
		for i := range ta {
			b := make([]byte, len(a.elems))
			for j := range b {
				b[j] = a.elems[j].(*StringValue).s[i]
			}
			ta[i] = makeString(string(b))
		}
		return makeArray(ta)
	default:
		panic(fmt.Sprintf("cannot transpose array of %T", a.elems[0]))
	}
}

func builtinRotate(n *IntegerValue, v Value) Value {
	if it, ok := v.(*IteratorValue); ok {
		v = builtinCollect(it)
	}
	switch v := v.(type) {
	case *ArrayValue:
		elems := make([]Value, 0, len(v.elems))
		elems = append(elems, v.elems[n.i:]...)
		elems = append(elems, v.elems[:n.i]...)
		return makeArray(elems)
	case *StringValue:
		return makeString(v.s[n.i:] + v.s[:n.i])
	default:
		panic(fmt.Sprintf("cannot rotate %T", v))
	}
}

func builtinCaesar(n *IntegerValue, s *StringValue) *StringValue {
	b := []byte(s.s)
	for i, c := range b {
		if 'a' <= c && c <= 'z' {
			b[i] = byte((int64(c-'a')+n.i)%26) + 'a'
		} else if 'A' <= c && c <= 'Z' {
			b[i] = byte((int64(c-'A')+n.i)%26) + 'Z'
		}
	}
	return makeString(string(b))
}

func builtinSort(v Value) Value {
	if it, ok := v.(*IteratorValue); ok {
		v = builtinCollect(it)
	}
	switch v := v.(type) {
	case *ArrayValue:
		sorted := append([]Value(nil), v.elems...)
		sort.Slice(sorted, func(i, j int) bool {
			return internalLess(sorted[i], sorted[j])
		})
		return makeArray(sorted)
	case *StringValue:
		b := []byte(v.s)
		sort.Slice(b, func(i int, j int) bool {
			return b[i] < b[j]
		})
		return makeString(string(b))
	}
	panic(fmt.Sprintf("cannot sort %T", v))
}

func builtinSortBy(env *Environment, fn Value, v Value) Value {
	if m, ok := v.(*MapValue); ok {
		v = internalMapIterator(m)
	}
	if it, ok := v.(*IteratorValue); ok {
		v = builtinCollect(it)
	}
	var arity int
	if _, ok := fn.(*ArrayValue); ok {
		arity = 0
	} else {
		arity = env.apply(fn).(*PartialValue).need()
	}
	switch v := v.(type) {
	case *ArrayValue:
		sorted := append([]Value(nil), v.elems...)
		var sortFn func(i, j int) bool
		switch arity {
		case 0:
			// multisort
			fns := fn.(*ArrayValue).elems
			sortFn = func(i, j int) bool {
				for _, fn := range fns {
					var cmp1, cmp2 bool
					arity := env.apply(fn).(*PartialValue).need()
					if arity == 1 {
						ei, ej := env.apply1(fn, sorted[i]), env.apply1(fn, sorted[j])
						cmp1 = internalLess(ei, ej)
						cmp2 = internalLess(ej, ei)
					} else {
						cmp1 = env.apply(fn, sorted[i], sorted[j]).(*BoolValue).b
						cmp2 = env.apply(fn, sorted[j], sorted[i]).(*BoolValue).b
					}
					if cmp1 != cmp2 {
						return cmp1
					}
				}
				return false
			}
		case 1:
			sortFn = func(i, j int) bool {
				return internalLess(env.apply1(fn, sorted[i]), env.apply1(fn, sorted[j]))
			}
		case 2:
			sortFn = func(i, j int) bool {
				return env.apply(fn, sorted[i], sorted[j]).(*BoolValue).b
			}
		}
		sort.Slice(sorted, sortFn)
		return makeArray(sorted)
	case *StringValue:
		b := []byte(v.s)
		sort.Slice(b, func(i int, j int) bool {
			return env.apply(fn, makeString(v.s[i:][:1]), makeString(v.s[j:][:1])).(*BoolValue).b
		})
		return makeString(string(b))
	}
	panic(fmt.Sprintf("cannot sort %T", v))
}

func builtinSorted(it *IteratorValue) *BoolValue {
	cur := it.next()
	sorted := true
	for v := it.next(); v != nil && sorted; v = it.next() {
		sorted = !internalLess(v, cur)
		cur = v
	}
	return makeBool(sorted)
}

func builtinSortedBy(env *Environment, fn Value, it *IteratorValue) *BoolValue {
	cur := it.next()
	sorted := true
	for v := it.next(); v != nil && sorted; v = it.next() {
		sorted = env.apply(fn, cur, v).(*BoolValue).b
		cur = v
	}
	return makeBool(sorted)
}

func builtinMap(env *Environment, fn Value, it *IteratorValue) *IteratorValue {
	return &IteratorValue{
		next: func() Value {
			v := it.next()
			if v == nil {
				return nil
			}
			return env.apply1(fn, v)
		},
		infinite: it.infinite,
	}
}

func builtinConcatmap(env *Environment, fn Value, it *IteratorValue) *IteratorValue {
	cur := &IteratorValue{next: func() Value { return nil }}
	return &IteratorValue{
		next: func() Value {
		again:
			n := cur.next()
			if n == nil {
				v := it.next()
				if v == nil {
					return nil
				}
				cur = internalToIterator(env.apply1(fn, v))
				goto again
			}
			return n
		},
		infinite: it.infinite,
	}
}

func builtinSum(it *IteratorValue) Value {
	sum := it.next()
	if sum == nil {
		return makeInteger(0)
	}
	for v := it.next(); v != nil; v = it.next() {
		sum = builtinAdd(sum, v)
	}
	return sum
}

func builtinProduct(it *IteratorValue) Value {
	prod := it.next()
	if prod == nil {
		return makeInteger(0)
	}
	for v := it.next(); v != nil; v = it.next() {
		prod = builtinMultiply(prod, v)
	}
	return prod
}

func builtinFirst(env *Environment, fn Value, it *IteratorValue) Value {
	for v := it.next(); v != nil; v = it.next() {
		v = internalClone(v)
		if internalTruthy(env.apply1(fn, v)) {
			return v
		}
	}
	panic("first: no elements satisfy predicate")
}

func builtinCount(env *Environment, c Value, it Value) *IntegerValue {
	switch it := it.(type) {
	case *StringValue:
		switch c := c.(type) {
		case *StringValue:
			return makeInteger(int64(strings.Count(it.s, c.s)))
		}
	case *ArrayValue:
		var n int64
		switch c.(type) {
		case *BuiltinValue, *PartialValue:
			for _, v := range it.elems {
				if internalTruthy(env.apply1(c, v)) {
					n++
				}
			}
		default:
			for _, v := range it.elems {
				if internalEquals(c, v) {
					n++
				}
			}
		}
		return makeInteger(n)
	case *IteratorValue:
		var n int64
		switch c.(type) {
		case *BuiltinValue, *PartialValue:
			for v := it.next(); v != nil; v = it.next() {
				if env.apply1(c, v).(*BoolValue).b {
					n++
				}
			}
		default:
			for v := it.next(); v != nil; v = it.next() {
				if internalEquals(c, v) {
					n++
				}
			}
		}
		return makeInteger(n)
	}
	panic(fmt.Sprintf("count: unhandled type combination %T %T", c, it))
}

func builtinLen(it *IteratorValue) *IntegerValue {
	if it.infinite {
		panic("len: infinite iterator")
	}
	var n int64
	for v := it.next(); v != nil; v = it.next() {
		n++
	}
	return makeInteger(n)
}

func builtinFold(env *Environment, fn Value, acc Value, it *IteratorValue) Value {
	for v := it.next(); v != nil; v = it.next() {
		acc = env.apply(fn, acc, v)
	}
	return acc
}

func builtinFold1(env *Environment, fn Value, it *IteratorValue) Value {
	acc := it.next()
	if acc == nil {
		panic("fold1: empty iterator")
	}
	return builtinFold(env, fn, acc, it)
}

func builtinScan(env *Environment, fn Value, it *IteratorValue) *IteratorValue {
	var acc Value
	return &IteratorValue{
		next: func() Value {
			if v := it.next(); v == nil {
				return nil
			} else if acc == nil {
				acc = v
			} else {
				acc = env.apply(fn, acc, v)
			}
			return acc
		},
		infinite: it.infinite,
	}
}

func builtinSet(env *Environment, key, val, in Value) Value {
	set := func(prev Value) Value {
		if fn, ok := val.(*PartialValue); ok {
			return env.apply(fn, prev)
		}
		return val
	}
	if it, ok := key.(*IteratorValue); ok {
		key = builtinCollect(it)
	}

	switch in := in.(type) {
	case *ArrayValue:
		in = internalClone(in).(*ArrayValue)
		switch key := key.(type) {
		case *IntegerValue:
			in.elems[key.i] = set(in.elems[key.i])
		case *ArrayValue:
			i := key.elems[len(key.elems)-1].(*IntegerValue).i
			key = makeArray(key.elems[:len(key.elems)-1])
			if len(key.elems) == 0 {
				in.elems[i] = set(in.elems[i])
			} else {
				in.elems[i] = builtinSet(env, key, val, in.elems[i])
			}
		default:
			panic(fmt.Sprintf("set: unhandled key type %T", key))
		}
		return in
	case *MapValue:
		in = internalClone(in).(*MapValue)
		in.set(key, set(in.get(key)))
		return in
	case *IteratorValue:
		switch key := key.(type) {
		case *IntegerValue:
			i := key.i + 1
			return &IteratorValue{
				next: func() Value {
					v := in.next()
					if v == nil {
						return nil
					} else if i--; i == 0 {
						return set(v)
					}
					return v
				},
			}
		case *ArrayValue:
			i := key.elems[len(key.elems)-1].(*IntegerValue).i + 1
			key = makeArray(key.elems[:len(key.elems)-1])
			return &IteratorValue{
				next: func() Value {
					v := in.next()
					if v == nil {
						return nil
					} else if i--; i == 0 {
						if len(key.elems) > 0 {
							return builtinSet(env, key, val, v)
						}
						return set(v)
					}
					return v
				},
			}

		default:
			panic(fmt.Sprintf("set: unhandled key type %T", key))
		}
	default:
		panic(fmt.Sprintf("set: invalid collection type %T", in))
	}
}

func builtinApply(env *Environment, fns *IteratorValue, v Value) Value {
	for fn := fns.next(); fn != nil; fn = fns.next() {
		v = env.apply1(fn, v)
	}
	return v
}

func builtinIterate(env *Environment, fn Value, v Value) *IteratorValue {
	first := true
	return &IteratorValue{
		next: func() Value {
			if !first {
				v = internalClone(v)
				v = env.apply1(fn, v)
			}
			first = false
			return v
		},
		infinite: true,
	}
}

func builtinStabilize(env *Environment, fn Value, v Value) Value {
	for {
		next := internalClone(v)
		next = env.apply1(fn, next)
		if internalEquals(v, next) {
			return v
		}
		v = next
	}
}

func builtinCycle(a *ArrayValue) *IteratorValue {
	n := 0
	return &IteratorValue{
		next: func() Value {
			v := a.elems[n]
			n = (n + 1) % len(a.elems)
			return v
		},
		infinite: true,
	}
}

func builtinConcat(it *IteratorValue) Value {
	switch acc := it.next().(type) {
	case *ArrayValue:
		elems := append([]Value(nil), acc.elems...)
		for v := it.next(); v != nil; v = it.next() {
			switch v := v.(type) {
			case *ArrayValue:
				elems = append(elems, v.elems...)
			case *IteratorValue:
				for vv := v.next(); vv != nil; vv = v.next() {
					elems = append(elems, vv)
				}
			default:
				panic(fmt.Sprintf("cannot concat %T with %T", acc, v))
			}
		}
		return makeArray(elems)
	case *StringValue:
		s := acc.s
		for v := it.next(); v != nil; v = it.next() {
			if sv, ok := v.(*StringValue); ok {
				s += sv.s
			} else {
				panic(fmt.Sprintf("cannot concat %T with %T", acc, v))
			}
		}
		return makeString(s)
	case *IteratorValue:
		return &IteratorValue{
			next: func() Value {
				v := acc.next()
				for v == nil {
					next := it.next()
					if next == nil {
						return nil
					} else if _, ok := next.(*IteratorValue); !ok {
						panic(fmt.Sprintf("cannot concat %T with %T", acc, next))
					}
					acc = next.(*IteratorValue)
					v = acc.next()
				}
				return v
			},
		}
	case nil:
		return makeArray(nil)
	default:
		panic(fmt.Sprintf("cannot concat %T", acc))
	}
}

func builtinAppend(v Value, it Value) Value {
	switch it := it.(type) {
	case *ArrayValue:
		elems := make([]Value, 0, len(it.elems)+1)
		elems = append(elems, it.elems...)
		elems = append(elems, v)
		return makeArray(elems)
	case *IteratorValue:
		return &IteratorValue{
			next: func() Value {
				n := it.next()
				if n == nil {
					n = v
					v = nil
				}
				return n
			},
		}
	default:
		panic(fmt.Sprintf("cannot append to %T", it))
	}
}

func builtinPrepend(v Value, it Value) Value {
	switch it := it.(type) {
	case *ArrayValue:
		elems := make([]Value, 0, len(it.elems)+1)
		elems = append(elems, v)
		elems = append(elems, it.elems...)
		return makeArray(elems)
	case *IteratorValue:
		return &IteratorValue{
			next: func() Value {
				if v == nil {
					return it.next()
				}
				n := v
				v = nil
				return n
			},
		}
	default:
		panic(fmt.Sprintf("cannot prepend to %T", it))
	}
}

func builtinDelete(d Value, it Value) Value {
	switch it := it.(type) {
	case *StringValue:
		ds, ok := d.(*StringValue)
		if !ok {
			panic(fmt.Sprintf("cannot delete %T from %T", d, it))
		}
		return makeString(strings.Replace(it.s, ds.s, "", -1))
	case *ArrayValue:
		var elems []Value
		for _, v := range it.elems {
			if !internalEquals(v, d) {
				elems = append(elems, v)
			}
		}
		return makeArray(elems)
	case *IteratorValue:
		return &IteratorValue{
			next: func() Value {
				v := it.next()
				for v != nil && internalEquals(v, d) {
					v = it.next()
				}
				return v
			},
			infinite: it.infinite,
		}
	default:
		panic(fmt.Sprintf("cannot delete from %T", it))
	}
}

func builtinDeleteAll(ds *ArrayValue, it Value) Value {
	switch it := it.(type) {
	case *StringValue:
		pairs := make([]string, len(ds.elems)*2)
		for i := range ds.elems {
			s, ok := ds.elems[i].(*StringValue)
			if !ok {
				panic(fmt.Sprintf("cannot delete %T from %T", ds.elems[i], it))
			}
			pairs[i*2] = s.s
		}
		return makeString(strings.NewReplacer(pairs...).Replace(it.s))
	case *ArrayValue:
		var elems []Value
	outer:
		for _, v := range it.elems {
			for _, d := range ds.elems {
				if internalEquals(v, d) {
					continue outer
				}
			}
			elems = append(elems, v)
		}
		return makeArray(elems)
	case *IteratorValue:
		return &IteratorValue{
			next: func() Value {
			again:
				v := it.next()
				for _, d := range ds.elems {
					if internalEquals(v, d) {
						goto again
					}
				}
				return v
			},
			infinite: it.infinite,
		}
	default:
		panic(fmt.Sprintf("cannot delete from %T", it))
	}
}

func builtinUniq(it *IteratorValue) *ArrayValue {
	if it.infinite {
		panic("cannot collect infinite iterator")
	}
	m := makeMap()
	for v := it.next(); v != nil; v = it.next() {
		m.set(v, nil)
	}
	return makeArray(m.keys)
}

func builtinUniqBy(env *Environment, fn Value, it *IteratorValue) *ArrayValue {
	if it.infinite {
		panic("cannot collect infinite iterator")
	}
	m := makeMap()
	for v := it.next(); v != nil; v = it.next() {
		k := env.apply1(fn, v)
		if m.get(k) == nil {
			m.set(k, v)
		}
	}
	return makeArray(m.vals)
}

func builtinDups(it *IteratorValue) *IteratorValue {
	seen := make(map[valueHash]int)
	return &IteratorValue{
		next: func() Value {
		again:
			v := it.next()
			if v == nil {
				return nil
			}
			h := v.hash()
			if seen[h]++; seen[h] > 1 {
				return v
			}
			goto again
		},
		infinite: it.infinite,
	}
}

func builtinFirstRepeat(it *IteratorValue) Value {
	return builtinHead(builtinDups(it))
}

func builtinFlatten(it *IteratorValue) *IteratorValue {
	v := it.next()
	vit := internalToIterator(v)
	return &IteratorValue{
		next: func() Value {
			n := vit.next()
			for n == nil {
				v = it.next()
				if v == nil {
					return nil
				}
				vit = internalToIterator(v)
				n = vit.next()
			}
			return n
		},
		infinite: it.infinite,
	}
}

func builtinDeltas(it *IteratorValue) *IteratorValue {
	prev := it.next()
	if prev == nil {
		panic("deltas: empty iterator")
	}
	return &IteratorValue{
		next: func() Value {
			v := it.next()
			if v == nil {
				return nil
			}
			d := builtinSub(v, prev)
			prev = v
			return d
		},
		infinite: it.infinite,
	}
}

func builtinDiff(a, b *IteratorValue) *IteratorValue {
	return &IteratorValue{
		next: func() Value {
		again:
			av, bv := a.next(), b.next()
			if av == nil {
				return nil
			} else if bv != nil && internalEquals(av, bv) {
				goto again
			}
			return av
		},
		infinite: a.infinite,
	}
}

func builtinSame(a, b *IteratorValue) *IteratorValue {
	return &IteratorValue{
		next: func() Value {
		again:
			av, bv := a.next(), b.next()
			if av == nil || bv == nil {
				return nil
			} else if !internalEquals(av, bv) {
				goto again
			}
			return av
		},
		infinite: a.infinite,
	}
}

func builtinZip(a, b *IteratorValue) *IteratorValue {
	return &IteratorValue{
		next: func() Value {
			av, bv := a.next(), b.next()
			if av == nil || bv == nil {
				return nil
			}
			return makeArray([]Value{av, bv})
		},
		infinite: a.infinite && b.infinite,
	}
}

func builtinZipWith(env *Environment, fn Value, a, b *IteratorValue) *IteratorValue {
	return &IteratorValue{
		next: func() Value {
			av, bv := a.next(), b.next()
			if av == nil || bv == nil {
				return nil
			}
			return env.apply(fn, av, bv)
		},
		infinite: a.infinite && b.infinite,
	}
}

func builtinAny(env *Environment, fn Value, it *IteratorValue) *BoolValue {
	for v := it.next(); v != nil; v = it.next() {
		if env.apply1(fn, v).(*BoolValue).b {
			return makeBool(true)
		}
	}
	return makeBool(false)
}

func builtinAbs(env *Environment, v *IntegerValue) *IntegerValue {
	if v.i < 0 {
		return makeInteger(-v.i)
	}
	return v
}

func builtinAdj(pos *ArrayValue) *ArrayValue {
	px := pos.elems[0].(*IntegerValue).i
	py := pos.elems[1].(*IntegerValue).i
	return makeArray([]Value{
		makeArray([]Value{makeInteger(px - 1), makeInteger(py + 0)}),
		makeArray([]Value{makeInteger(px + 0), makeInteger(py - 1)}),
		makeArray([]Value{makeInteger(px + 0), makeInteger(py + 1)}),
		makeArray([]Value{makeInteger(px + 1), makeInteger(py + 0)}),
	})
}

func builtinAdj8(pos *ArrayValue) *ArrayValue {
	px := pos.elems[0].(*IntegerValue).i
	py := pos.elems[1].(*IntegerValue).i
	return makeArray([]Value{
		makeArray([]Value{makeInteger(px - 1), makeInteger(py - 1)}),
		makeArray([]Value{makeInteger(px - 1), makeInteger(py + 0)}),
		makeArray([]Value{makeInteger(px - 1), makeInteger(py + 1)}),
		makeArray([]Value{makeInteger(px + 0), makeInteger(py - 1)}),
		makeArray([]Value{makeInteger(px + 0), makeInteger(py + 1)}),
		makeArray([]Value{makeInteger(px + 1), makeInteger(py - 1)}),
		makeArray([]Value{makeInteger(px + 1), makeInteger(py + 0)}),
		makeArray([]Value{makeInteger(px + 1), makeInteger(py + 1)}),
	})
}

func builtinWithin(dims *ArrayValue, pos *ArrayValue) *BoolValue {
	dx := dims.elems[0].(*IntegerValue).i
	dy := dims.elems[1].(*IntegerValue).i
	px := pos.elems[0].(*IntegerValue).i
	py := pos.elems[1].(*IntegerValue).i
	return makeBool(0 <= px && px < dx && 0 <= py && py < dy)
}

func builtinAll(env *Environment, fn Value, it *IteratorValue) *BoolValue {
	for v := it.next(); v != nil; v = it.next() {
		if !internalTruthy(env.apply1(fn, v)) {
			return makeBool(false)
		}
	}
	return makeBool(true)
}

func builtinNone(env *Environment, fn Value, it *IteratorValue) *BoolValue {
	for v := it.next(); v != nil; v = it.next() {
		if internalTruthy(env.apply1(fn, v)) {
			return makeBool(false)
		}
	}
	return makeBool(true)
}

func builtinRepeat(v Value) *IteratorValue {
	return &IteratorValue{
		next:     func() Value { return v },
		infinite: true,
	}
}

func builtinAlpha() *StringValue {
	return makeString("abcdefghijklmnopqrstuvwxyz")
}

func builtinIota() *IteratorValue {
	n := int64(-1)
	return &IteratorValue{
		next: func() Value {
			n++
			return makeInteger(n)
		},
		infinite: true,
	}
}

func builtinFromDigits(it *IteratorValue) *IntegerValue {
	var b []byte
	for v := it.next(); v != nil; v = it.next() {
		i := v.(*IntegerValue).i
		if i > 9 {
			panic("fromDigits: invalid digit")
		}
		b = append(b, '0'+byte(i))
	}
	n, err := strconv.ParseInt(string(b), 10, 64)
	if err != nil {
		panic(err)
	}
	return makeInteger(n)
}

func builtinFromBase(base *IntegerValue, s *StringValue) *IntegerValue {
	n, err := strconv.ParseInt(s.s, int(base.i), 64)
	if err != nil {
		panic(err)
	}
	return makeInteger(n)
}

func builtinToBase(base *IntegerValue, i *IntegerValue) *StringValue {
	return makeString(strconv.FormatInt(i.i, int(base.i)))
}

func builtinEnum(start, end Value) *IteratorValue {
	switch start := start.(type) {
	case *IntegerValue:
		n := start.i
		n--
		return &IteratorValue{
			next: func() Value {
				if n+1 >= end.(*IntegerValue).i {
					return nil
				}
				n++
				return makeInteger(n)
			},
		}
	case *ArrayValue:
		n0 := start.elems[0].(*IntegerValue).i
		n1 := start.elems[1].(*IntegerValue).i
		e0 := end.(*ArrayValue).elems[0].(*IntegerValue).i
		e1 := end.(*ArrayValue).elems[1].(*IntegerValue).i
		n0--
		return &IteratorValue{
			next: func() Value {
				if n0+1 >= e0 && n1+1 >= e1 {
					return nil
				}
				if n0++; n0 >= e0 {
					n0 = 0
					n1++
				}
				return makeArray([]Value{makeInteger(n0), makeInteger(n1)})
			},
		}
	}
	panic("enum: invalid arguments")
}

func builtinAssoc(it *IteratorValue) *MapValue {
	m := makeMap()
	for {
		k, v := it.next(), it.next()
		if k == nil {
			break
		} else if v == nil {
			panic("assoc: dangling key")
		}
		m.set(k, v)
	}
	return m
}

func builtinInvert(m *MapValue) *MapValue {
	im := makeMap()
	for i := range m.vals {
		a := im.getOr(m.vals[i], makeArray(nil)).(*ArrayValue)
		a.elems = append(a.elems, m.keys[i])
		im.set(m.vals[i], a)
	}
	return im
}

func builtinGraph(it *IteratorValue) *MapValue {
	m := makeMap()
	for {
		v := it.next()
		if v == nil {
			break
		}
		a := builtinCollect(internalToIterator(v))
		mv := m.getOr(a.elems[0], makeMap()).(*MapValue)
		mv.set(a.elems[1], makeBool(true))
		m.set(a.elems[0], mv)
	}
	return m
}

func builtinKeys(m *MapValue) *ArrayValue {
	return makeArray(append([]Value(nil), m.keys...))
}

func builtinVals(m *MapValue) *ArrayValue {
	return makeArray(append([]Value(nil), m.vals...))
}

func builtinHasKey(e Value, m *MapValue) *BoolValue {
	return makeBool(m.get(e) != nil)
}

func builtinHasVal(e Value, m *MapValue) *BoolValue {
	for _, v := range m.vals {
		if internalEquals(v, e) {
			return makeBool(true)
		}
	}
	return makeBool(false)
}

func builtinContains(c Value, it Value) *BoolValue {
	switch it := it.(type) {
	case *StringValue:
		s, ok := c.(*StringValue)
		if !ok {
			panic(fmt.Sprintf("cannot check for %T in %T", c, it))
		}
		return makeBool(strings.Contains(it.s, s.s))
	case *ArrayValue:
		for _, v := range it.elems {
			if internalEquals(v, c) {
				return makeBool(true)
			}
		}
		return makeBool(false)
	case *MapValue:
		return makeBool(it.get(c) != nil)
	case *IteratorValue:
		for v := it.next(); v != nil; v = it.next() {
			if internalEquals(v, c) {
				return makeBool(true)
			}
		}
		return makeBool(false)
	default:
		panic(fmt.Sprintf("contains: unhandled type %T", it))
	}
}

func builtinIn(it Value, c Value) *BoolValue {
	return builtinContains(c, it)
}

func builtinDims(v Value) *ArrayValue {
	switch v := v.(type) {
	default:
		return makeArray(nil)
	case *ArrayValue:
		if len(v.elems) == 0 {
			return makeArray([]Value{makeInteger(0)})
		}
		inner := builtinDims(v.elems[0])
		return makeArray(append(inner.elems, makeInteger(int64(len(v.elems)))))

	case *IteratorValue:
		// TODO
		return builtinDims(builtinCollect(v))
	}
}

func builtinDFS(env *Environment, fn Value, acc Value, start Value) Value {
	var rec func(acc Value, path []Value) Value
	rec = func(acc Value, path []Value) Value {
		r := env.apply(fn, acc, path[len(path)-1], makeArray(path)).(*ArrayValue)
		acc = r.elems[0]
		next, ok := r.elems[1].(*ArrayValue)
		if !ok {
			next = builtinCollect(r.elems[1].(*IteratorValue))
		}
		for _, e := range next.elems {
			acc = rec(acc, append(path, e))
		}
		return acc
	}
	return rec(acc, []Value{start})
}

func builtinFlood(env *Environment, fn Value, p *ArrayValue) *ArrayValue {
	seen := make(map[string]bool)
	var visited []Value
	queue := []*ArrayValue{p}
	for len(queue) > 0 {
		p, queue = queue[0], queue[1:]
		if seen[p.String()] {
			continue
		}
		seen[p.String()] = true
		visited = append(visited, p)
		for _, a := range builtinAdj(p).elems {
			if internalTruthy(env.apply(fn, a)) {
				queue = append(queue, a.(*ArrayValue))
			}
		}
	}
	return makeArray(visited)
}

func builtinExhaust(env *Environment, fn Value, q *ArrayValue, v Value) Value {
	seen := make(map[valueHash]bool)
	queue := q.elems
	for len(queue) > 0 {
		e := queue[0]
		queue = queue[1:]
		if seen[e.hash()] {
			continue
		}
		seen[e.hash()] = true
		r, ok := env.apply(fn, v, e).(*ArrayValue)
		if !ok {
			panic("exhaust: non-array result")
		}
		v = r.elems[0]
		if it, ok := r.elems[1].(*IteratorValue); ok {
			r.elems[1] = builtinCollect(it)
		}
		queue = append(queue, r.elems[1].(*ArrayValue).elems...)
	}
	return v
}

func builtinContainsAny(cs *ArrayValue, it Value) Value {
	switch it := it.(type) {
	case *StringValue:
		for i := range cs.elems {
			s, ok := cs.elems[i].(*StringValue)
			if !ok {
				panic(fmt.Sprintf("cannot check for %T in %T", cs.elems[i], it))
			}
			if strings.Contains(it.s, s.s) {
				return makeBool(true)
			}
		}
		return makeBool(false)
	case *ArrayValue:
		for _, v := range it.elems {
			for _, c := range cs.elems {
				if internalEquals(v, c) {
					return makeBool(true)
				}
			}
		}
		return makeBool(false)
	case *IteratorValue:
		for v := it.next(); v != nil; v = it.next() {
			for _, c := range cs.elems {
				if internalEquals(v, c) {
					return makeBool(true)
				}
			}
		}
		return makeBool(false)
	default:
		panic(fmt.Sprintf("containsAll: unhandled type %T", it))
	}
}

func builtinHasPrefix(p Value, it *IteratorValue) *BoolValue {
	pit := internalToIterator(p)
	if pit.infinite {
		return makeBool(false)
	}
	for v := pit.next(); v != nil; v = pit.next() {
		if iv := it.next(); iv == nil || !internalEquals(v, iv) {
			return makeBool(false)
		}
	}
	return makeBool(true)
}

func builtinHistogram(it *IteratorValue) *MapValue {
	m := makeMap()
	for v := it.next(); v != nil; v = it.next() {
		n := m.getOr(v, makeInteger(0)).(*IntegerValue)
		n.i++
		m.set(v, n)
	}
	return m
}

func builtinMove(cmd *ArrayValue, pos *ArrayValue) *ArrayValue {
	pos = internalClone(pos).(*ArrayValue)
	switch cmd.elems[0].(*StringValue).s {
	case "u":
		pos.elems[1] = builtinAdd(pos.elems[1], cmd.elems[1])
	case "d":
		pos.elems[1] = builtinSub(pos.elems[1], cmd.elems[1])
	case "l":
		pos.elems[0] = builtinSub(pos.elems[0], cmd.elems[1])
	case "r":
		pos.elems[0] = builtinAdd(pos.elems[0], cmd.elems[1])
	default:
		panic("move: invalid direction")
	}
	return pos
}

func builtinDraw(pos, dest *ArrayValue) *IteratorValue {
	dx := dest.elems[0].(*IntegerValue).i - pos.elems[0].(*IntegerValue).i
	dy := dest.elems[1].(*IntegerValue).i - pos.elems[1].(*IntegerValue).i
	// gcd
	a, b := dx, dy
	if a < 0 {
		a = -a
	}
	if b < 0 {
		b = -b
	}
	for b != 0 {
		a, b = b, a%b
	}
	gcd := a
	stepX := makeInteger(dx / gcd)
	stepY := makeInteger(dy / gcd)

	next := internalClone(pos).(*ArrayValue)
	return &IteratorValue{
		next: func() Value {
			if internalEquals(pos, dest) {
				return nil
			}
			pos = internalClone(next).(*ArrayValue)
			next = makeArray([]Value{
				builtinAdd(next.elems[0], stepX),
				builtinAdd(next.elems[1], stepY),
			})
			return pos
		},
	}
}

func builtinMax(it *IteratorValue) Value {
	max := it.next()
	if max == nil {
		panic("max: empty iterator")
	}
	for v := it.next(); v != nil; v = it.next() {
		if internalLess(max, v) {
			max = v
		}
	}
	return max
}

func builtinMin(it *IteratorValue) Value {
	min := it.next()
	if min == nil {
		panic("min: empty iterator")
	}
	for v := it.next(); v != nil; v = it.next() {
		if internalLess(v, min) {
			min = v
		}
	}
	return min
}

func builtinMaxBy(env *Environment, fn Value, it *IteratorValue) Value {
	var maxIn, maxOut Value
	for v := it.next(); v != nil; v = it.next() {
		v = internalClone(v)
		out := env.apply1(fn, v)
		if maxIn == nil || internalLess(maxOut, out) {
			maxIn, maxOut = v, out
		}
	}
	return maxIn
}

func builtinMinBy(env *Environment, fn Value, it *IteratorValue) Value {
	var maxIn, maxOut Value
	for v := it.next(); v != nil; v = it.next() {
		v = internalClone(v)
		out := env.apply1(fn, v)
		if maxIn == nil || internalLess(out, maxOut) {
			maxIn, maxOut = v, out
		}
	}
	return maxIn
}

func builtinMaxIndex(it Value) Value {
	var max, maxIndex Value
	switch it := it.(type) {
	case *ArrayValue:
		for i, v := range it.elems {
			if max == nil || internalLess(max, v) {
				max = v
				maxIndex = makeInteger(int64(i))
			}
		}
	case *MapValue:
		for i := range it.keys {
			if max == nil || internalLess(max, it.vals[i]) {
				max = it.vals[i]
				maxIndex = it.keys[i]
			}
		}
	case *IteratorValue:
		var i int64
		for v := it.next(); v != nil; v = it.next() {
			if max == nil || internalLess(max, v) {
				max = v
				maxIndex = makeInteger(int64(i))
			}
			i++
		}
	default:
		panic(fmt.Sprintf("maxIndex: expected array or map, got %T", it))
	}
	if maxIndex == nil {
		panic("maxIndex: empty")
	}
	return maxIndex
}

func builtinMinIndex(it Value) Value {
	var min, minIndex Value
	switch it := it.(type) {
	case *ArrayValue:
		for i, v := range it.elems {
			if min == nil || internalLess(v, min) {
				min = v
				minIndex = makeInteger(int64(i))
			}
		}
	case *MapValue:
		for i := range it.keys {
			if min == nil || internalLess(it.vals[i], min) {
				min = it.vals[i]
				minIndex = it.keys[i]
			}
		}
	case *IteratorValue:
		var i int64
		for v := it.next(); v != nil; v = it.next() {
			if min == nil || internalLess(v, min) {
				min = v
				minIndex = makeInteger(i)
			}
			i++
		}
	default:
		panic(fmt.Sprintf("minIndex: expected array or map, got %T", it))
	}
	if minIndex == nil {
		panic("minIndex: empty")
	}
	return minIndex
}

func builtinMean(it *IteratorValue) *IntegerValue {
	var n, sum int64
	for v := it.next(); v != nil; v = it.next() {
		sum += v.(*IntegerValue).i
		n++
	}
	return makeInteger(sum / n)
}

func builtinMedian(it *IteratorValue) *IntegerValue {
	a := builtinSort(it).(*ArrayValue)
	v := a.elems[len(a.elems)/2].(*IntegerValue)
	if len(a.elems)%2 == 0 {
		v.i = (v.i + a.elems[len(a.elems)/2+1].(*IntegerValue).i) / 2
	}
	return v
}

func builtinMemo(env *Environment, fn Value) *BuiltinValue {
	m := makeMap()
	return &BuiltinValue{
		name:  "memo",
		nargs: env.apply(fn).(*PartialValue).need(),
		fn: func(env *Environment, args []Value) Value {
			if v := m.get(makeArray(args)); v != nil {
				return v
			}
			v := env.apply(fn, args...)
			m.set(makeArray(args), v)
			return v
		},
	}
}
