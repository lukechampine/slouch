package bytecode

import (
	"fmt"
	"slices"
	"strconv"
	"strings"
)

type builtinFunc struct {
	Arity    int
	Comptime bool // can be called without a VM
	Fn       func(*VM, []Value) Value
}

func makeBuiltinUnary[T, U Value](fn func(T) U) builtinFunc {
	return builtinFunc{
		Arity:    1,
		Comptime: true,
		Fn: func(_ *VM, args []Value) Value {
			return fn(args[0].(T))
		},
	}
}

func makeBuiltinBinary[T, U, V Value](fn func(T, U) V) builtinFunc {
	return builtinFunc{
		Arity:    2,
		Comptime: true,
		Fn: func(_ *VM, args []Value) Value {
			return fn(args[0].(T), args[1].(U))
		},
	}
}

var builtins map[string]builtinFunc

func init() {
	builtins = map[string]builtinFunc{
		"map": {
			Arity: 2,
			Fn: func(vm *VM, args []Value) Value {
				fn := args[0].(ValFunc)
				ice := toIcicle(args[1])
				return &ValIcicle{
					next: func(i int) Value {
						v := ice.get(i)
						if v == nil {
							return nil
						}
						return vm.Call(fn, []Value{v})
					},
				}
			},
		},

		"choose": {
			Arity: 2,
			Fn: func(_ *VM, args []Value) Value {
				n := args[0].(ValInt)
				a := toArray(args[1])
				if n > ValInt(len(a)) {
					panic("cannot choose more elements than array length")
				}
				choices := make([]int, n)
				for i := range choices {
					choices[i] = i
				}
				choices[len(choices)-1]--
				return &ValIcicle{
					next: func(_ int) Value {
						for i := len(choices) - 1; i >= 0; i-- {
							choices[i]++
							if choices[i] < len(a)-len(choices[i+1:]) {
								for i++; i < len(choices); i++ {
									choices[i] = choices[i-1] + 1
								}
								break
							} else if i == 0 {
								return nil
							}
						}
						elems := make([]Value, n)
						for i, c := range choices {
							elems[i] = a[c]
						}
						return ValArray(elems)
					},
				}
			},
		},

		"count": {
			Arity: 2,
			Fn: func(vm *VM, args []Value) Value {
				fn := args[0].(ValFunc)
				i := 0
				for _, v := range toArray(args[1]) {
					if vm.Call(fn, []Value{v}).(ValBool) {
						i++
					}
				}
				return ValInt(int64(i))
			},
		},

		"enum": makeBuiltinBinary(func(start, end ValInt) *ValIcicle {
			return &ValIcicle{
				next: func(i int) Value {
					if int64(i)+int64(start) >= int64(end) {
						return nil
					}
					return ValInt(int64(start) + int64(i))
				},
			}
		}),

		"filter": {
			Arity: 2,
			Fn: func(vm *VM, args []Value) Value {
				fn := args[0].(ValFunc)
				ice := toIcicle(args[1])
				i := 0
				return &ValIcicle{
					next: func(int) Value {
						for {
							v := ice.get(i)
							if v == nil {
								return nil
							}
							i++
							if vm.Call(fn, []Value{v}).(ValBool) {
								return v
							}
						}
					},
				}
			},
		},

		// TODO: should be a macro
		"first": {
			Arity: 2,
			Fn: func(vm *VM, args []Value) Value {
				return builtins["filter"].Fn(vm, args).(*ValIcicle).get(0)
			},
		},

		"ints": makeBuiltinUnary(func(s ValString) ValArray {
			fs := strings.FieldsFunc(string(s), func(r rune) bool {
				return !('0' <= r && r <= '9') && r != '-' // 0-9 or -
			})
			elems := make(ValArray, 0, len(fs))
			for _, w := range fs {
				if i, err := strconv.Atoi(w); err == nil {
					elems = append(elems, ValInt(int64(i)))
				}
			}
			return elems
		}),

		"len": {
			Arity: 1,
			Fn: func(_ *VM, args []Value) Value {
				switch v := args[0].(type) {
				case ValString:
					return ValInt(int64(len(v)))
				case ValArray:
					return ValInt(int64(len(v)))
				case ValAssoc:
					return ValInt(int64(len(v.indices)))
				case *ValIcicle:
					n := v.len()
					for v.get(n) != nil {
						n++
					}
					return ValInt(int64(n))
				default:
					panic(fmt.Sprintf("invalid argument to len: %v", v))
				}
			},
		},

		"max": {
			Arity:    1,
			Comptime: true,
			Fn: func(_ *VM, args []Value) Value {
				vals := toArray(args[0])
				m := vals[0]
				for _, v := range vals[1:] {
					if internalLess(m, v) {
						m = v
					}
				}
				return m
			},
		},

		"pow": makeBuiltinBinary(func(a, b ValInt) ValInt {
			r := ValInt(1)
			for range b {
				r *= a
			}
			return r
		}),

		"product": {
			Arity:    1,
			Comptime: true,
			Fn: func(_ *VM, args []Value) Value {
				vals := toArray(args[0])
				if len(vals) == 0 {
					return ValInt(0)
				}
				prod := ValInt(1)
				for _, v := range vals {
					prod *= v.(ValInt)
				}
				return prod
			},
		},

		"reverse": {
			Arity:    1,
			Comptime: true,
			Fn: func(_ *VM, args []Value) Value {
				switch v := args[0].(type) {
				case ValString:
					rev := []byte(v)
					slices.Reverse(rev)
					return ValString(rev)
				case ValArray:
					rev := slices.Clone(v)
					slices.Reverse(rev)
					return rev
				case *ValIcicle:
					vals := v.Collect()
					slices.Reverse(vals)
					return vals
				default:
					panic(fmt.Sprintf("invalid argument to len: %v", v))
				}
			},
		},

		"string": makeBuiltinUnary(func(v Value) ValString {
			return ValString(v.String())
		}),

		"sum": {
			Arity:    1,
			Comptime: true,
			Fn: func(_ *VM, args []Value) Value {
				vals := toArray(args[0])
				sum := ValInt(0)
				for _, v := range vals {
					sum += v.(ValInt)
				}
				return sum
			},
		},

		"tail": {
			Arity:    1,
			Comptime: true,
			Fn: func(_ *VM, args []Value) Value {
				switch v := args[0].(type) {
				case ValString:
					return v[1:]
				case ValArray:
					return v[1:]
				default:
					panic("invalid argument to tail")
				}
			},
		},

		"take": {
			Arity:    2,
			Comptime: true,
			Fn: func(_ *VM, args []Value) Value {
				n := args[0].(ValInt)
				switch v := args[1].(type) {
				case ValString:
					return v[:n]
				case ValArray:
					return v[:n]
				case *ValIcicle:
					return &ValIcicle{
						next: func(i int) Value {
							if i >= int(n) {
								return nil
							}
							return v.next(i)
						},
					}
				default:
					panic("invalid argument to take")
				}
			},
		},

		"toUpper": makeBuiltinUnary(func(s ValString) ValString {
			return ValString(strings.ToUpper(string(s)))
		}),

		"tr": {
			Arity: 2,
			Fn: func(_ *VM, args []Value) Value {
				m := args[0].(ValAssoc)
				if s, ok := args[1].(ValString); ok {
					str := true
					for _, v := range m.vals {
						if _, ok := v.(ValString); !ok {
							str = false
							break
						}
					}
					if str {
						pairs := make([]string, 0, len(m.vals)*2)
						for _, i := range m.indices {
							pairs = append(pairs, string(m.keys[i].(ValString)), string(m.vals[i].(ValString)))
						}
						return ValString(strings.NewReplacer(pairs...).Replace(string(s)))
					}
				}
				ice := toIcicle(args[1])
				return &ValIcicle{
					next: func(i int) Value {
						v := ice.get(i)
						if v == nil {
							return nil
						}
						return m.getOr(v, v)
					},
				}
			},
		},
	}
}
