package bytecode

import (
	"fmt"
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
				for _, v := range toIcicle(args[1]).collect() {
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
				vals := toIcicle(args[0]).collect()
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
				vals := toIcicle(args[0]).collect()
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
					rev := make([]byte, len(v))
					for i := range rev {
						rev[i] = v[len(v)-i-1]
					}
					return ValString(rev)
				case ValArray:
					rev := make([]Value, len(v))
					for i := range rev {
						rev[i] = v[len(v)-i-1]
					}
					return ValArray(rev)
				case *ValIcicle:
					vals := v.collect()
					return &ValIcicle{
						next: func(i int) Value {
							if i >= len(vals) {
								return nil
							}
							return vals[len(vals)-i-1]
						},
					}
				default:
					panic(fmt.Sprintf("invalid argument to len: %v", v))
				}
			},
		},

		"string": makeBuiltinUnary(func(v Value) ValString {
			return ValString(v.String())
		}),

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
					panic("invalid argument to len")
				}
			},
		},

		"toUpper": makeBuiltinUnary(func(s ValString) ValString {
			return ValString(strings.ToUpper(string(s)))
		}),
	}
}
