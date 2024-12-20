package bytecode

import "strings"

type builtinFunc struct {
	Arity int
	Fn    func([]Value) Value
}

var builtins = map[string]builtinFunc{
	"toUpper": {
		Arity: 1,
		Fn: func(args []Value) Value {
			return ValString(strings.ToUpper(string(args[0].(ValString))))
		},
	},

	"pow": {
		Arity: 2,
		Fn: func(args []Value) Value {
			r := ValInt(1)
			for i := ValInt(0); i < args[1].(ValInt); i++ {
				r *= args[0].(ValInt)
			}
			return r
		},
	},
}
