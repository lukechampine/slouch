// Package evaluator ...
package evaluator

import (
	"fmt"
	"io/ioutil"
	"sort"
	"strconv"
	"strings"
	"unicode"

	"lukechampine.com/slouch/ast"
	"lukechampine.com/slouch/lexer"
	"lukechampine.com/slouch/parser"
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

type ArraySentinel struct{}

func (ArraySentinel) isValue()       {}
func (ArraySentinel) String() string { return "[" }

type QuotationValue struct {
	body []ast.Expression
}

func (qv QuotationValue) isValue() {}
func (qv QuotationValue) String() string {
	strs := make([]string, len(qv.body))
	for i := range strs {
		strs[i] = qv.body[i].String()
	}
	return "( " + strings.Join(strs, " ") + " )"
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

type envFn func(*Environment)

type Environment struct {
	Stack []Value
	vars  map[string]envFn
}

func New() *Environment {
	return &Environment{
		vars: make(map[string]envFn),
	}
}

func (env *Environment) lookup(name string) (envFn, bool) {
	v, ok := env.vars[name]
	return v, ok
}

func (env *Environment) Eval(n ast.Expression) {
	env.computeEnvFn(n)(env)
}

func (env *Environment) pop() (v Value) {
	v, env.Stack = env.Stack[len(env.Stack)-1], env.Stack[:len(env.Stack)-1]
	return
}

func (env *Environment) push(v Value) {
	env.Stack = append(env.Stack, v)
}

func (env *Environment) computeEnvFn(n ast.Node) envFn {
	pushFn := func(v Value) envFn { return func(e *Environment) { e.push(v) } }

	switch n := n.(type) {
	case ast.Identifier:
		if fn, ok := env.vars[n.Name]; ok {
			return fn
		} else if fn, ok := builtins[n.Name]; ok {
			return fn
		}
		panic(fmt.Sprintf("unknown identifier"))

	case ast.Integer:
		i, _ := strconv.ParseInt(n.Value, 0, 64) // TODO: parse as bigint
		return pushFn(IntegerValue{i})

	case ast.String:
		return pushFn(StringValue{n.Value})

	case ast.Quotation:
		return pushFn(QuotationValue{n.Body})

	default:
		panic(fmt.Sprintf("couldn't evaluate type %T", n))
	}
}

// autocomplete
func (env *Environment) Do(line []rune, pos int) (completions [][]rune, length int) {
	// to type-check, we need to evaluate any preceeding expressions first, so
	// save a copy that we can restore later
	stack := append([]Value(nil), env.Stack...)
	defer func() { env.Stack = stack }()

	length = pos
	s := string(line[:pos])
	space := strings.LastIndexByte(s, ' ')
	s = s[space+1:]
	for b := range builtins {
		if strings.HasPrefix(b, s) {
			completions = append(completions, []rune(strings.TrimPrefix(b, s)))
		}
	}
	sort.Slice(completions, func(i, j int) bool {
		return string(completions[i]) < string(completions[j])
	})
	return
}

// sadly, must use init here to avoid cyclical references to Eval
var builtins map[string]envFn

func init() {
	builtins = map[string]envFn{
		"-":       builtinMinus,
		"[":       builtinStartArray,
		"]":       builtinEndArray,
		"*":       builtinMultiply,
		"/":       builtinDivide,
		"+":       builtinPlus,
		"apply":   builtinApply,
		"array":   builtinArray,
		"at":      builtinAt,
		"chars":   builtinChars,
		"count":   builtinCount,
		"drop":    builtinDrop,
		"dup":     builtinDup,
		"eval":    builtinEval,
		"eqn":     builtinEqn,
		"find":    builtinFind,
		"flatten": builtinFlatten,
		"fold":    builtinFold,
		"fold1":   builtinFold1,
		"ints":    builtinInts,
		"len":     builtinLen,
		"map":     builtinMap,
		"match":   builtinMatch,
		"reverse": builtinReverse,
		"slurp":   builtinSlurp,
		"scan1":   builtinScan1,
		"skip":    builtinSkip,
		"splat":   builtinSplat,
		"split":   builtinSplit,
		"sort":    builtinSort,
		"sum":     builtinSum,
		"swap":    builtinSwap,
		"take":    builtinTake,
		"upto":    builtinUpto,
		"with":    builtinWith,
		"wrap":    builtinWrap,
		"uniq":    builtinUniq,
		"zip":     builtinZip,
	}
}

func builtinDup(env *Environment) {
	v := env.pop()
	env.push(v)
	env.push(v)
}

func builtinSwap(env *Environment) {
	a := env.pop()
	b := env.pop()
	env.push(a)
	env.push(b)
}

func builtinDrop(env *Environment) {
	env.pop()
}

func builtinStartArray(env *Environment) {
	env.push(ArraySentinel{})
}

func builtinEndArray(env *Environment) {
	i := len(env.Stack) - 1
	for ; i >= 0; i-- {
		v := env.Stack[i]
		if _, ok := v.(ArraySentinel); ok {
			break
		}
	}
	elems := append([]Value(nil), env.Stack[i+1:]...)
	env.Stack = env.Stack[:i]
	env.push(ArrayValue{elems})
}

func builtinApply(env *Environment) {
	av, ok := env.pop().(ArrayValue)
	if !ok {
		panic("not an array")
	}
	v := env.pop()
	for _, qe := range av.elems {
		q, ok := qe.(QuotationValue)
		if !ok {
			panic("not a quotation")
		}
		env.push(v)
		for _, e := range q.body {
			env.Eval(e)
		}
	}
}

func builtinArray(env *Environment) {
	iv, ok := env.pop().(IntegerValue)
	if !ok {
		panic("not an integer")
	}
	i := len(env.Stack) - int(iv.i)
	elems := append([]Value(nil), env.Stack[i:]...)
	env.Stack = env.Stack[:i]
	env.push(ArrayValue{elems})
}

func builtinSplat(env *Environment) {
	av, ok := env.pop().(ArrayValue)
	if !ok {
		panic("not an array")
	}
	env.Stack = append(env.Stack, av.elems...)
}

func builtinSplit(env *Environment) {
	sep := env.pop()
	switch av := env.pop().(type) {
	case StringValue:
		s, ok := sep.(StringValue)
		if !ok {
			panic("strings can only be split by other strings")
		}
		elems := strings.Split(av.s, s.s)
		var split ArrayValue
		for _, e := range elems {
			split.elems = append(split.elems, StringValue{e})
		}
		env.push(split)
	case ArrayValue:
		var split ArrayValue
		var group ArrayValue
		for _, v := range av.elems {
			if equals(v, sep) {
				split.elems = append(split.elems, group)
				group = ArrayValue{}
			} else {
				group.elems = append(group.elems, v)
			}
		}
		split.elems = append(split.elems, group)
		env.push(split)
	default:
		panic("unhandled split type")
	}
}

func builtinAt(env *Environment) {
	iv, ok := env.pop().(IntegerValue)
	if !ok {
		panic("not an integer")
	}
	av, ok := env.pop().(ArrayValue)
	if !ok {
		panic("not an array")
	}
	if iv.i < 0 {
		iv.i = int64(len(av.elems)) + iv.i
	}
	env.push(av.elems[iv.i])
}

func builtinEval(env *Environment) {
	qv, ok := env.pop().(QuotationValue)
	if !ok {
		panic("not an integer")
	}
	for _, v := range qv.body {
		env.Eval(v)
	}
}

func builtinEqn(env *Environment) {
	sv, ok := env.pop().(StringValue)
	if !ok {
		panic("not a string")
	}
	// determine number of variables
	vars := make(map[rune]Value)
	for _, c := range sv.s {
		if 'a' <= c && c <= 'z' {
			if _, ok := vars[c]; !ok {
				vars[c] = env.pop()
			}
		}
	}

}

func builtinFind(env *Environment) {
	f := env.pop()
	av, ok := env.pop().(ArrayValue)
	if !ok {
		panic("not an array")
	}
	switch f := f.(type) {
	case IntegerValue:
		for i, e := range av.elems {
			if equals(e, f) {
				env.push(IntegerValue{int64(i)})
				return
			}
		}
		panic("not found") // TODO: what should be pushed here?
	default:
		panic("unhandled find type")
	}
}

func builtinInts(env *Environment) {
	sv, ok := env.pop().(StringValue)
	if !ok {
		panic("not a quotation")
	}
	fs := strings.FieldsFunc(sv.s, func(r rune) bool {
		return !unicode.IsDigit(r) && r != '-'
	})
	var iv ArrayValue
	for _, w := range fs {
		i, err := strconv.Atoi(w)
		if err == nil {
			iv.elems = append(iv.elems, IntegerValue{int64(i)})
		}
	}
	env.push(iv)
}

func builtinLen(env *Environment) {
	av, ok := env.pop().(ArrayValue)
	if !ok {
		panic("not an array")
	}
	env.push(IntegerValue{i: int64(len(av.elems))})
}

func builtinMap(env *Environment) {
	qv, ok := env.pop().(QuotationValue)
	if !ok {
		panic("not a quotation")
	}
	av, ok := env.pop().(ArrayValue)
	if !ok {
		panic("not an array")
	}
	builtinStartArray(env)
	for _, v := range av.elems {
		env.push(v)
		for _, e := range qv.body {
			env.Eval(e)
		}
	}
	builtinEndArray(env)
}

func builtinWrap(env *Environment) {
	iv, ok := env.pop().(IntegerValue)
	if !ok {
		panic("not an integer")
	}
	av := env.pop()
	switch av := av.(type) {
	case ArrayValue:
		builtinStartArray(env)
		for len(av.elems) > 0 {
			n := int(iv.i)
			if n > len(av.elems) {
				n = len(av.elems)
			}
			env.push(ArrayValue{append([]Value(nil), av.elems[:n]...)})
			av.elems = av.elems[n:]
		}
		builtinEndArray(env)

	case StringValue:
		builtinStartArray(env)
		for len(av.s) > 0 {
			n := int(iv.i)
			if n > len(av.s) {
				n = len(av.s)
			}
			env.push(StringValue{av.s[:n]})
			av.s = av.s[n:]
		}
		builtinEndArray(env)
	default:
		panic("invalid wrap type")
	}
}

func builtinMatch(env *Environment) {
	pat, ok := env.pop().(ArrayValue)
	if !ok {
		panic("not an array")
	}
	vals, ok := env.pop().(ArrayValue)
	if !ok {
		panic("not an array")
	}
	if len(pat.elems)%2 != 0 {
		panic("match patterns not evenly paired")
	}
	m := make(map[Value]Value)
	for i := 0; i < len(pat.elems); i += 2 {
		m[pat.elems[i]] = pat.elems[i+1]
	}
	builtinStartArray(env)
	for _, v := range vals.elems {
		v, ok := m[v]
		if !ok {
			panic("unhandled pattern")
		}
		env.push(v)
	}
	builtinEndArray(env)
}

func builtinFlatten(env *Environment) {
	var flatten func(av ArrayValue) ArrayValue
	flatten = func(av ArrayValue) ArrayValue {
		var f ArrayValue
		for _, v := range av.elems {
			switch v := v.(type) {
			case ArrayValue:
				f.elems = append(f.elems, flatten(v).elems...)
			default:
				f.elems = append(f.elems, v)
			}
		}
		return f
	}
	av, ok := env.pop().(ArrayValue)
	if !ok {
		panic("not an array")
	}
	env.push(flatten(av))
}

func builtinFold(env *Environment) {
	qv, ok := env.pop().(QuotationValue)
	if !ok {
		panic("not a quotation")
	}
	av, ok := env.pop().(ArrayValue)
	if !ok {
		panic("not an array")
	}
	for _, v := range av.elems {
		env.push(v)
		for _, e := range qv.body {
			env.Eval(e)
		}
	}
}

func builtinFold1(env *Environment) {
	qv, ok := env.pop().(QuotationValue)
	if !ok {
		panic("not a quotation")
	}
	av, ok := env.pop().(ArrayValue)
	if !ok {
		panic("not an array")
	}
	env.push(av.elems[0])
	for _, v := range av.elems[1:] {
		env.push(v)
		for _, e := range qv.body {
			env.Eval(e)
		}
	}
}

func builtinScan1(env *Environment) {
	qv, ok := env.pop().(QuotationValue)
	if !ok {
		panic("not a quotation")
	}
	av, ok := env.pop().(ArrayValue)
	if !ok {
		panic("not an array")
	}
	env.push(av.elems[0])
	for _, v := range av.elems[1:] {
		builtinDup(env)
		env.push(v)
		for _, e := range qv.body {
			env.Eval(e)
		}
	}
	env.push(IntegerValue{int64(len(av.elems))})
	builtinArray(env)
}

func builtinReverse(env *Environment) {
	av, ok := env.pop().(ArrayValue)
	if !ok {
		panic("not a string")
	}
	var r ArrayValue
	for i := range av.elems {
		r.elems = append(r.elems, av.elems[len(av.elems)-i-1])
	}
	env.push(r)
}

func builtinSlurp(env *Environment) {
	sv, ok := env.pop().(StringValue)
	if !ok {
		panic("not a string")
	}
	data, err := ioutil.ReadFile(sv.s)
	if err != nil {
		panic(err)
	}
	env.push(StringValue{string(data)})
}

func builtinChars(env *Environment) {
	env.push(IntegerValue{1})
	builtinWrap(env)
}

func builtinCount(env *Environment) {
	var n int64
	cv := env.pop()
	switch arr := env.pop().(type) {
	case ArrayValue:
		for _, e := range arr.elems {
			if equals(e, cv) {
				n++
			}
		}
	case StringValue:
		sv, ok := cv.(StringValue)
		if !ok {
			panic("not a string")
		}
		n = int64(strings.Count(arr.s, sv.s))
	default:
		panic("invalid type for count")
	}
	env.push(IntegerValue{n})
}

func builtinWith(env *Environment) {
	q, ok := env.pop().(QuotationValue)
	if !ok {
		panic("not an array")
	}
	v := env.pop()
	// TODO: this is ghastly
	q.body = append(parser.Parse(lexer.Tokenize(v.String())).Expressions, q.body...)
	env.push(q)
}

func builtinTake(env *Environment) {
	n, ok := env.pop().(IntegerValue)
	if !ok {
		panic("not an integer")
	}
	av, ok := env.pop().(ArrayValue)
	if !ok {
		panic("not an array")
	}
	if int64(len(av.elems)) < n.i {
		n.i = int64(len(av.elems))
	}
	env.push(ArrayValue{elems: append([]Value(nil), av.elems[:n.i]...)})
}

func builtinSkip(env *Environment) {
	n, ok := env.pop().(IntegerValue)
	if !ok {
		panic("not an integer")
	}
	av, ok := env.pop().(ArrayValue)
	if !ok {
		panic("not an array")
	}
	if int64(len(av.elems)) < n.i {
		n.i = int64(len(av.elems))
	}
	env.push(ArrayValue{elems: append([]Value(nil), av.elems[n.i:]...)})
}

func builtinUpto(env *Environment) {
	iv, ok := env.pop().(IntegerValue)
	if !ok {
		panic("not an integer")
	}
	inc := int64(1)
	if iv.i < 0 {
		inc = -1
	}
	var av ArrayValue
	i := IntegerValue{0}
	for i != iv {
		av.elems = append(av.elems, i)
		i.i += inc
	}
	env.push(av)
}

func builtinSort(env *Environment) {
	av, ok := env.pop().(ArrayValue)
	if !ok {
		panic("not an array")
	}
	s := ArrayValue{elems: append([]Value(nil), av.elems...)}
	sort.Slice(s.elems, func(i, j int) bool {
		switch ai := s.elems[i].(type) {
		case IntegerValue:
			aj, ok := s.elems[j].(IntegerValue)
			if !ok {
				panic("cannot sort array containing different types")
			}
			return ai.i < aj.i
		case StringValue:
			aj, ok := s.elems[j].(StringValue)
			if !ok {
				panic("cannot sort array containing different types")
			}
			return ai.s < aj.s
		default:
			panic("not an array of integers or strings")
		}
	})
	env.push(s)
}

func builtinSum(env *Environment) {
	av, ok := env.pop().(ArrayValue)
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
	env.push(IntegerValue{sum})
}

func builtinUniq(env *Environment) {
	a, ok := env.pop().(ArrayValue)
	if !ok {
		panic("not an array")
	}
	var u ArrayValue
	seen := make(map[string]struct{})
	for _, e := range a.elems {
		s := e.String()
		if _, ok := seen[s]; !ok {
			seen[s] = struct{}{}
			u.elems = append(u.elems, e)
		}
	}
	env.push(u)
}

func builtinZip(env *Environment) {
	q, ok := env.pop().(QuotationValue)
	if !ok {
		panic("not an array")
	}
	a1, ok := env.pop().(ArrayValue)
	if !ok {
		panic("not an array")
	}
	a2, ok := env.pop().(ArrayValue)
	if !ok {
		panic("not an array")
	}
	if len(a1.elems) != len(a2.elems) {
		panic("array length mismatch")
	}
	for i := range a1.elems {
		env.push(a1.elems[i])
		env.push(a2.elems[i])
		for _, e := range q.body {
			env.Eval(e)
		}
	}
	env.push(IntegerValue{int64(len(a1.elems))})
	builtinArray(env)
}

func builtinPlus(env *Environment) {
	r, l := env.pop(), env.pop()
	env.push(opPlus(l, r))
}

func builtinMinus(env *Environment) {
	r, l := env.pop(), env.pop()
	env.push(opMinus(l, r))
}

func builtinMultiply(env *Environment) {
	r, l := env.pop(), env.pop()
	env.push(opMultiply(l, r))
}

func builtinDivide(env *Environment) {
	r, l := env.pop(), env.pop()
	env.push(opPlus(l, r))
}

func opPlus(l, r Value) Value {
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
				l.elems[i] = opPlus(le, r)
			}
			return l

		case StringValue:
			for i, le := range l.elems {
				l.elems[i] = opPlus(le, r)
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

func opMinus(l, r Value) Value {
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
				l.elems[i] = opMinus(le, r)
			}
			return l

		case StringValue:
			for i, le := range l.elems {
				l.elems[i] = opMinus(le, r)
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

func opMultiply(l, r Value) Value {
	switch l := l.(type) {
	case IntegerValue:
		switch r := r.(type) {
		case IntegerValue:
			return IntegerValue{l.i * r.i}
		default:
			panic("illegal multiply types")
		}

	case ArrayValue:
		switch r := r.(type) {
		case IntegerValue:
			for i, le := range l.elems {
				l.elems[i] = opMultiply(le, r)
			}
			return l

		case ArrayValue:
			var matrix ArrayValue
			for i := range l.elems {
				lv, ok := l.elems[i].(IntegerValue)
				if !ok {
					panic("can only multiply arrays of integers")
				}
				var row ArrayValue
				for j := range r.elems {
					rv, ok := r.elems[j].(IntegerValue)
					if !ok {
						panic("can only multiply arrays of integers")
					}
					row.elems = append(row.elems, IntegerValue{lv.i * rv.i})
				}
				matrix.elems = append(matrix.elems, row)
			}
			return matrix

		default:
			panic("illegal multiply types")
		}

	default:
		panic("unhandled multiply type")
	}
}
