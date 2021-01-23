package evaluator

import (
	"fmt"
	"reflect"
	"sort"
	"strconv"
	"strings"

	"lukechampine.com/slouch/token"
)

var infixFns = map[token.Kind]*BuiltinValue{
	token.Plus:          makeBuiltin("+", builtinAdd),
	token.Neg:           makeBuiltin("-", builtinSub),
	token.Star:          makeBuiltin("*", builtinMultiply),
	token.Slash:         makeBuiltin("/", builtinDivide),
	token.Equals:        makeBuiltin("==", builtinEquals),
	token.NotEquals:     makeBuiltin("!=", builtinNotEquals),
	token.Less:          makeBuiltin("<", builtinLess),
	token.Greater:       makeBuiltin(">", builtinGreater),
	token.LessEquals:    makeBuiltin("<=", builtinLessEquals),
	token.GreaterEquals: makeBuiltin(">=", builtinGreaterEquals),
	token.And:           makeBuiltin("and", builtinAnd),
	token.Or:            makeBuiltin("or", builtinOr),
	token.Dot:           makeBuiltin(".", builtinDot),
}

var builtins = [...]*BuiltinValue{
	makeBuiltin("choose", builtinChoose),
	makeBuiltin("first", builtinFirst),
	makeBuiltin("count", builtinCount),
	makeBuiltin("len", builtinLen),
	makeBuiltin("head", builtinHead),
	makeBuiltin("lines", builtinLines),
	makeBuiltin("words", builtinWords),
	makeBuiltin("digits", builtinDigits),
	makeBuiltin("ints", builtinInts),
	makeBuiltin("map", builtinMap),
	makeBuiltin("reverse", builtinReverse),
	makeBuiltin("transpose", builtinTranspose),
	makeBuiltin("sort", builtinSort),
	makeBuiltin("sortBy", builtinSortBy),
	makeBuiltin("sorted", builtinSorted),
	makeBuiltin("sortedBy", builtinSortedBy),
	makeBuiltin("sum", builtinSum),
	makeBuiltin("product", builtinProduct),
	makeBuiltin("toUpper", builtinToUpper),
	makeBuiltin("delete", builtinDelete),
	makeBuiltin("deleteAll", builtinDeleteAll),
	makeBuiltin("take", builtinTake),
	makeBuiltin("drop", builtinDrop),
	makeBuiltin("takeWhile", builtinTakeWhile),
	makeBuiltin("dropWhile", builtinDropWhile),
	makeBuiltin("filter", builtinFilter),
	makeBuiltin("window", builtinWindow),
	makeBuiltin("runs", builtinRuns),
	makeBuiltin("partition", builtinPartition),
	makeBuiltin("collect", builtinCollect),
	makeBuiltin("contains", builtinContains),
	makeBuiltin("scan", builtinScan),
	makeBuiltin("iterate", builtinIterate),
	makeBuiltin("stabilize", builtinStabilize),
	makeBuiltin("firstrepeat", builtinFirstRepeat),
	makeBuiltin("diff", builtinDiff),
	makeBuiltin("same", builtinSame),
	makeBuiltin("zip", builtinZip),
	makeBuiltin("zipWith", builtinZipWith),
	makeBuiltin("any", builtinAny),
	makeBuiltin("all", builtinAll),
	makeBuiltin("cycle", builtinCycle),
	makeBuiltin("concat", builtinConcat),
	makeBuiltin("uniq", builtinUniq),
	makeBuiltin("dups", builtinDups),
	makeBuiltin("repeat", builtinRepeat),
	makeBuiltin("alpha", builtinAlpha),
	makeBuiltin("iota", builtinIota),
	makeBuiltin("enum", builtinEnum),
	makeBuiltin("assoc", builtinAssoc),
	makeBuiltin("keys", builtinKeys),
	makeBuiltin("vals", builtinVals),
	makeBuiltin("hasKey", builtinHasKey),
	makeBuiltin("hasVal", builtinHasVal),
	makeBuiltin("histogram", builtinHistogram),
	makeBuiltin("max", builtinMax),
	makeBuiltin("min", builtinMin),
	makeBuiltin("maxBy", builtinMaxBy),
	makeBuiltin("minBy", builtinMinBy),
	makeBuiltin("maxIndex", builtinMaxIndex),
	makeBuiltin("minIndex", builtinMinIndex),
}

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
	return &BuiltinValue{
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
				} else if !arg.Type().AssignableTo(t.In(i)) {
					panic(fmt.Sprintf("%v expected %v, got %v", name, t.In(i), arg.Type()))
				}
			}
			return v.Call(argVals)[0].Interface().(Value)
		},
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
		panic("unreachable")
		//panic(fmt.Sprintf("truthy: unhandled type %T", v))
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
	panic(fmt.Sprintf("cannot compare %T to %T", a, b))
}

func internalEquals(l, r Value) bool {
	switch l := l.(type) {
	case *IntegerValue:
		switch r := r.(type) {
		case *IntegerValue:
			return l.i == r.i
		}
	case *StringValue:
		switch r := r.(type) {
		case *StringValue:
			return l.s == r.s
		}
	}
	panic(fmt.Sprintf("unhandled type combination %T + %T", l, r))
}

func internalClone(v Value) Value {
	if it, ok := v.(*IteratorValue); ok {
		return builtinCollect(it)
	}
	return v
}

func builtinCollect(it *IteratorValue) *ArrayValue {
	if it.infinite {
		panic("can't collect infinite iterator")
	}
	var elems []Value
	for e := it.next(); e != nil; e = it.next() {
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
	panic(fmt.Sprintf("unhandled type combination %T + %T", l, r))
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
	panic(fmt.Sprintf("unhandled type combination %T * %T", l, r))
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

func builtinAnd(l, r Value) *BoolValue {
	return makeBool(internalTruthy(l) && internalTruthy(r))
}

func builtinOr(l, r Value) *BoolValue {
	return makeBool(internalTruthy(l) || internalTruthy(r))
}

func builtinDot(l, r Value) Value {
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
	}
	panic(fmt.Sprintf("unhandled type combination %T + %T", l, r))
}

func builtinLines(s StringValue) *ArrayValue {
	lines := strings.Split(strings.TrimSpace(s.s), "\n")
	elems := make([]Value, len(lines))
	for i := range elems {
		elems[i] = makeString(lines[i])
	}
	return makeArray(elems)
}

func builtinWords(s StringValue) *ArrayValue {
	words := strings.Fields(strings.TrimSpace(s.s))
	elems := make([]Value, len(words))
	for i := range elems {
		elems[i] = makeString(words[i])
	}
	return makeArray(elems)
}

func builtinDigits(v *IntegerValue) *ArrayValue {
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
}

func builtinInts(s StringValue) *ArrayValue {
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

func builtinHead(it *IteratorValue) Value {
	v := it.next()
	if v == nil {
		panic("head called on empty array or iterator")
	}
	return v
}

func builtinToUpper(s *StringValue) *StringValue {
	return makeString(strings.ToUpper(s.s))
}

func builtinTake(n *IntegerValue, it IteratorValue) *IteratorValue {
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
			if !internalTruthy(env.apply(fn, c)) {
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
		if internalTruthy(env.apply(fn, c)) {
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
			if !internalTruthy(env.apply(fn, c)) {
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

func builtinWindow(n *IntegerValue, it *IteratorValue) *IteratorValue {
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

func builtinPartition(n *IntegerValue, it IteratorValue) *IteratorValue {
	return &IteratorValue{
		next: func() Value {
			a := builtinCollect(builtinTake(n, it))
			if len(a.elems) == 0 {
				return nil
			}
			return a
		},
	}
}

func builtinChoose(n *IntegerValue, a ArrayValue) *IteratorValue {
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

func builtinReverse(v Value) Value {
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

func builtinSort(v Value) Value {
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
	switch v := v.(type) {
	case *ArrayValue:
		sorted := append([]Value(nil), v.elems...)
		sort.Slice(sorted, func(i, j int) bool {
			return env.apply(fn, sorted[i], sorted[j]).(*BoolValue).b
		})
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
			return env.apply(fn, v)
		},
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
		if internalTruthy(env.apply(fn, v)) {
			return v
		}
	}
	panic("first: no elements satisfy predicate")
}

func builtinCount(env *Environment, fn Value, it *IteratorValue) *IntegerValue {
	var n int64
	for v := it.next(); v != nil; v = it.next() {
		if internalTruthy(env.apply(fn, v)) {
			n++
		}
	}
	return makeInteger(n)
}

func builtinLen(it *IteratorValue) *IntegerValue {
	var n int64
	for v := it.next(); v != nil; v = it.next() {
		n++
	}
	return makeInteger(n)
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

func builtinIterate(env *Environment, fn Value, v Value) *IteratorValue {
	return &IteratorValue{
		next: func() Value {
			v = internalClone(v)
			v = env.apply(fn, v)
			return v
		},
		infinite: true,
	}
}

func builtinStabilize(env *Environment, fn Value, v Value) Value {
	for {
		next := internalClone(v)
		next = env.apply(fn, next)
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
		for v := it.next(); v != nil; v = it.next() {
			if sv, ok := v.(*StringValue); ok {
				acc.s += sv.s
			} else {
				panic(fmt.Sprintf("cannot concat %T with %T", acc, v))
			}
		}
		return acc
	case nil:
		return makeArray(nil)
	default:
		panic(fmt.Sprintf("cannot concat %T", acc))
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

func builtinDiff(a, b IteratorValue) *IteratorValue {
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
		infinite: a.infinite || b.infinite,
	}
}

func builtinSame(a, b IteratorValue) *IteratorValue {
	return &IteratorValue{
		next: func() Value {
		again:
			av, bv := a.next(), b.next()
			if av == nil {
				return nil
			} else if bv != nil && !internalEquals(av, bv) {
				goto again
			}
			return av
		},
		infinite: a.infinite || b.infinite,
	}
}

func builtinZip(a, b IteratorValue) *IteratorValue {
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

func builtinZipWith(env *Environment, fn Value, a, b IteratorValue) *IteratorValue {
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
		if internalTruthy(env.apply(fn, v)) {
			return makeBool(true)
		}
	}
	return makeBool(false)
}

func builtinAll(env *Environment, fn Value, it *IteratorValue) *BoolValue {
	for v := it.next(); v != nil; v = it.next() {
		if !internalTruthy(env.apply(fn, v)) {
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

func builtinEnum(start, end *IntegerValue) *IteratorValue {
	n := start.i
	n--
	return &IteratorValue{
		next: func() Value {
			if n >= end.i {
				return nil
			}
			n++
			return makeInteger(n)
		},
	}
}

func builtinAssoc(a *ArrayValue) *MapValue {
	if len(a.elems)%2 != 0 {
		panic("dangling key in assoc")
	}
	m := makeMap()
	for i := 0; i < len(a.elems); i += 2 {
		m.set(a.elems[i], a.elems[i+1])
	}
	return m
}

func builtinKeys(m *MapValue) *ArrayValue {
	return makeArray(append([]Value(nil), m.keys...))
}

func builtinVals(m MapValue) *ArrayValue {
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

func builtinContains(e Value, it IteratorValue) *BoolValue {
	for v := it.next(); v != nil; v = it.next() {
		if internalEquals(v, e) {
			return makeBool(true)
		}
	}
	return makeBool(false)
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

func builtinMax(it *IteratorValue) Value {
	var max Value
	for v := it.next(); v != nil; v = it.next() {
		if max == nil || internalLess(max, v) {
			max = v
		}
	}
	return max
}

func builtinMin(it *IteratorValue) Value {
	var min Value
	for v := it.next(); v != nil; v = it.next() {
		if min == nil || internalLess(v, min) {
			min = v
		}
	}
	return min
}

func builtinMaxBy(env *Environment, fn Value, it *IteratorValue) Value {
	var maxIn, maxOut Value
	for v := it.next(); v != nil; v = it.next() {
		v = internalClone(v)
		out := env.apply(fn, v)
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
		out := env.apply(fn, v)
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
