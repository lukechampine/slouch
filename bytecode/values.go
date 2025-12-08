package bytecode

import (
	"fmt"
	"strconv"
	"strings"

	"lukechampine.com/slouch/token"
)

type Value interface {
	isValue()
	fmt.Stringer
}

type ValBool bool
type ValInt int64
type ValString string
type ValArray []Value
type ValFunc struct {
	Name    string
	Mod     token.Kind // for splat/rep
	Arity   int
	Applied []Value
}

func (v ValFunc) Need() int {
	switch v.Mod {
	case token.Splat, token.Rep:
		return min(1, v.Arity-len(v.Applied))
	default:
		return v.Arity - len(v.Applied)
	}
}

func (ValBool) isValue()           {}
func (ValInt) isValue()            {}
func (ValString) isValue()         {}
func (ValArray) isValue()          {}
func (ValFunc) isValue()           {}
func (v ValBool) String() string   { return strconv.FormatBool(bool(v)) }
func (v ValInt) String() string    { return strconv.FormatInt(int64(v), 10) }
func (v ValString) String() string { return strconv.Quote(string(v)) }
func (v ValArray) String() string {
	var sb strings.Builder
	sb.WriteByte('[')
	for i, e := range v {
		if i > 0 {
			sb.WriteString(", ")
		}
		sb.WriteString(e.String())
	}
	sb.WriteByte(']')
	return sb.String()
}
func (v ValFunc) String() string {
	args := make([]string, v.Arity)
	for i := 0; i < v.Arity; i++ {
		if i < len(v.Applied) {
			args[i] = v.Applied[i].String()
		} else {
			args[i] = "_"
		}
	}
	mod := ""
	switch v.Mod {
	case token.Splat:
		mod = "-<"
	case token.Rep:
		mod = "-:"
	}
	return fmt.Sprintf("%v%v(%v)", mod, v.Name, strings.Join(args, ","))
}

func assocHash(v Value) string {
	s := v.String()
	switch v.(type) {
	case ValBool:
		return "bool:" + s
	case ValInt:
		return "int:" + s
	case ValString:
		return "string:" + s
	case ValArray:
		return "array:" + s
	case *ValAssoc:
		return "assoc:" + s
	default:
		panic(fmt.Sprintf("unhandled assoc key type %T", v))
	}
}

type ValAssoc struct {
	indices    map[string]int
	keys, vals []Value
}

func (ValAssoc) isValue() {}
func (m ValAssoc) String() string {
	var strs []string
	for i := range m.keys {
		strs = append(strs, fmt.Sprintf("%v:%v", m.keys[i], m.vals[i]))
	}
	return "[" + strings.Join(strs, ", ") + "]"
}

func (m ValAssoc) get(k Value) Value {
	return m.getOr(k, nil)
}

func (m ValAssoc) getOr(k, d Value) Value {
	i, ok := m.indices[assocHash(k)]
	if !ok {
		return d
	}
	return m.vals[i]
}

func (m *ValAssoc) set(k, v Value) {
	h := assocHash(k)
	if i, ok := m.indices[h]; ok {
		m.vals[i] = v
	} else {
		m.keys = append(m.keys, k)
		m.vals = append(m.vals, v)
		m.indices[h] = len(m.keys) - 1
	}
}

func makeAssoc(elems []Value) ValAssoc {
	av := ValAssoc{indices: make(map[string]int)}
	for i := 0; i < len(elems); i += 2 {
		av.set(elems[i], elems[i+1])
	}
	return av
}

type ValIcicle struct {
	vals []Value
	next func(i int) Value
}

func (*ValIcicle) isValue() {}
func (ice *ValIcicle) String() string {
	return fmt.Sprintf("<icicle, len %v>", ice.len())
}

func (ice *ValIcicle) get(i int) Value {
	for ice.len() <= i {
		if ice.next == nil {
			return nil
		}
		v := ice.next(ice.len())
		if v == nil {
			ice.next = nil
			return nil
		}
		ice.vals = append(ice.vals, v)
	}
	return ice.vals[i]
}

func (ice *ValIcicle) len() int {
	return len(ice.vals)
}

func (ice *ValIcicle) Collect() ValArray {
	ice.get(1e18)
	return ValArray(ice.vals)
}

func toIcicle(v Value) *ValIcicle {
	switch v := v.(type) {
	case *ValIcicle:
		return v
	case ValArray:
		return &ValIcicle{
			vals: v,
		}
	case ValString:
		return &ValIcicle{next: func(i int) Value {
			if i >= len(v) {
				return nil
			}
			return ValString(v[i : i+1])
		}}
	default:
		panic(fmt.Sprintf("unhandled type %T", v))
	}
}

func toArray(v Value) ValArray {
	return toIcicle(v).Collect()
}

func evalPrefixOp(op token.Kind, a Value) Value {
	switch op {
	case token.Negative:
		return ValInt(-a.(ValInt))
	default:
		panic(fmt.Sprintf("unhandled prefix operator %v", op))
	}
}

func evalInfixOp(op token.Kind, a, b Value) Value {
	switch op {
	case token.Plus:
		switch a.(type) {
		case ValInt:
			return ValInt(a.(ValInt) + b.(ValInt))
		case ValString:
			return ValString(a.(ValString) + b.(ValString))
		default:
			panic("+: unhandled operand types")
		}
	case token.Minus:
		switch a.(type) {
		case ValInt:
			return ValInt(a.(ValInt) - b.(ValInt))
		default:
			panic("-: unhandled operand types")
		}
	case token.Star:
		switch a.(type) {
		case ValInt:
			return ValInt(a.(ValInt) * b.(ValInt))
		default:
			panic("*: unhandled operand types")
		}
	case token.Slash:
		switch a.(type) {
		case ValInt:
			return ValInt(a.(ValInt) / b.(ValInt))
		default:
			panic("/: unhandled operand types")
		}
	case token.And:
		switch a.(type) {
		case ValBool:
			return ValBool(a.(ValBool) && b.(ValBool))
		default:
			panic("and: unhandled operand types")
		}
	case token.Or:
		switch a.(type) {
		case ValBool:
			return ValBool(a.(ValBool) || b.(ValBool))
		default:
			panic("or: unhandled operand types")
		}
	case token.Equals:
		switch a := a.(type) {
		case ValArray:
			b, ok := b.(ValArray)
			if !ok || len(a) != len(b) {
				return ValBool(false)
			}
			for i := range a {
				if !evalInfixOp(token.Equals, a[i], b[i]).(ValBool) {
					return ValBool(false)
				}
			}
			return ValBool(true)
		case ValAssoc:
			b, ok := b.(ValAssoc)
			if !ok || len(a.indices) != len(b.indices) {
				return ValBool(false)
			}
			for i := range a.keys {
				if !evalInfixOp(token.Equals, a.keys[i], b.keys[i]).(ValBool) ||
					!evalInfixOp(token.Equals, a.vals[i], b.vals[i]).(ValBool) {
					return ValBool(false)
				}
			}
			return ValBool(true)
		case ValBool, ValInt, ValString:
			return ValBool(a == b)
		default:
			panic("==: unhandled operand types")
		}
	case token.NotEquals:
		return !evalInfixOp(token.Equals, a, b).(ValBool)
	case token.Dot:
		switch a := a.(type) {
		case ValArray:
			switch b := b.(type) {
			case ValInt:
				return a[b]
			}
		case ValString:
			switch b := b.(type) {
			case ValInt:
				return ValString(a[b : b+1])
			}
		case *ValIcicle:
			switch b := b.(type) {
			case ValInt:
				return a.get(int(b))
			}
		case ValAssoc:
			v := a.get(b)
			if v == nil {
				// TODO: maybe return false?
				panic(fmt.Sprintf("map does not contain %v", b))
			}
			return v
		}
		panic(fmt.Sprintf("unhandled type combination %T . %T", a, b))
	case token.Greater:
		return ValBool(internalLess(b, a))
	case token.Less:
		return ValBool(internalLess(a, b))
	case token.GreaterEquals:
		return ValBool(!internalLess(a, b))
	case token.LessEquals:
		return ValBool(!internalLess(b, a))
	case token.DivisibleBy:
		switch a.(type) {
		case ValInt:
			return ValBool(a.(ValInt)%b.(ValInt) == 0)
		default:
			panic("%?: unhandled operand types")
		}
	default:
		panic(fmt.Sprintf("unhandled infix operator %v", op))
	}
}

func internalLess(a, b Value) bool {
	switch a := a.(type) {
	case ValInt:
		if b, ok := b.(ValInt); ok {
			return a < b
		}
	case ValString:
		if b, ok := b.(ValString); ok {
			return a < b
		}
	}
	panic(fmt.Sprintf("cannot order %T relative to %T", a, b))
}

func internalTruthy(v Value) bool {
	switch v := v.(type) {
	case ValInt:
		return v != 0
	case ValString:
		return v != ""
	case ValBool:
		return bool(v)
	case ValArray:
		return len(v) != 0
	default:
		panic(fmt.Sprintf("truthy: unhandled type %T", v))
	}
}
