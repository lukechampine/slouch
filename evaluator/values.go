package evaluator

import (
	"fmt"
	"strconv"
	"strings"
)

type valueHash string

type Value interface {
	hash() valueHash
	fmt.Stringer
}

type IntegerValue struct {
	i int64 // TODO: make this arbitrary-precision
}

func (iv *IntegerValue) hash() valueHash { return valueHash(iv.String()) }
func (iv *IntegerValue) String() string  { return strconv.FormatInt(iv.i, 10) }

func makeInteger(i int64) *IntegerValue { return &IntegerValue{i: i} }

type StringValue struct {
	s string
}

func (sv *StringValue) hash() valueHash { return valueHash(sv.String()) }
func (sv *StringValue) String() string  { return strconv.Quote(sv.s) }

func makeString(s string) *StringValue { return &StringValue{s: s} }

type BoolValue struct {
	b bool
}

func (bv *BoolValue) hash() valueHash { return valueHash(bv.String()) }
func (bv *BoolValue) String() string  { return strconv.FormatBool(bv.b) }

func makeBool(b bool) *BoolValue { return &BoolValue{b: b} }

type HoleValue struct{}

func (hv HoleValue) hash() valueHash { return valueHash(hv.String()) }
func (hv HoleValue) String() string  { return "_" }

func isHole(v Value) bool {
	_, ok := v.(HoleValue)
	return v == nil || ok
}

type ArrayValue struct {
	elems []Value
}

func (av *ArrayValue) hash() valueHash { return valueHash(av.String()) }
func (av *ArrayValue) String() string {
	strs := make([]string, len(av.elems))
	for i := range strs {
		strs[i] = av.elems[i].String()
	}
	return "[" + strings.Join(strs, ", ") + "]"
}

func makeArray(elems []Value) *ArrayValue { return &ArrayValue{elems: elems} }

type MapValue struct {
	indices    map[valueHash]int
	keys, vals []Value
}

func (m *MapValue) hash() valueHash { return valueHash(m.String()) }
func (m *MapValue) String() string {
	var strs []string
	for i := range m.keys {
		strs = append(strs, fmt.Sprintf("%v:%v", m.keys[i], m.vals[i]))
	}
	return "[" + strings.Join(strs, " ") + "]"
}

func (m MapValue) get(k Value) Value {
	return m.getOr(k, nil)
}

func (m MapValue) getOr(k, d Value) Value {
	i, ok := m.indices[k.hash()]
	if !ok {
		return d
	}
	return m.vals[i]
}

func (m *MapValue) set(k, v Value) {
	h := k.hash()
	if i, ok := m.indices[h]; ok {
		m.vals[i] = v
	} else {
		m.keys = append(m.keys, k)
		m.vals = append(m.vals, v)
		m.indices[h] = len(m.keys) - 1
	}
}

func makeMap() *MapValue { return &MapValue{indices: make(map[valueHash]int)} }

type IteratorValue struct {
	next     func() Value
	infinite bool
}

func (iv *IteratorValue) hash() valueHash { return valueHash(iv.String()) }
func (iv *IteratorValue) String() string {
	if iv.infinite {
		return "<infinite iterator>"
	}
	return builtinCollect(iv).String()
}

type PartialValue struct {
	fn   Value
	args []Value // missing values represented by nil
}

func (pv *PartialValue) have() int {
	n := 0
	for _, a := range pv.args {
		if !isHole(a) {
			n++
		}
	}
	return n
}

func (pv *PartialValue) need() int { return len(pv.args) - pv.have() }

func (pv *PartialValue) hash() valueHash { return valueHash(pv.String()) }
func (pv *PartialValue) String() string {
	return fmt.Sprintf("<partial (%d-adic, have %v)>", len(pv.args), pv.have())
}

func makePartial(fn Value, args []Value) *PartialValue { return &PartialValue{fn: fn, args: args} }

type BuiltinValue struct {
	name  string
	nargs int
	fn    func(*Environment, []Value) Value
}

func (bv *BuiltinValue) hash() valueHash { return valueHash(bv.String()) }
func (bv *BuiltinValue) String() string {
	return fmt.Sprintf("<builtin %q (%d-adic)>", bv.name, bv.nargs)
}

type GoValue struct {
	name string
}

func (gv *GoValue) hash() valueHash { return valueHash(gv.String()) }
func (gv *GoValue) String() string  { return gv.name }
