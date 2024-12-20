package bytecode

import (
	"fmt"
	"testing"

	"lukechampine.com/slouch/evaluator"
	"lukechampine.com/slouch/lexer"
	"lukechampine.com/slouch/parser"
	"lukechampine.com/slouch/token"
)

func TestCases(t *testing.T) {
	tests := []struct {
		program string
		input   string
		output  string
	}{
		{
			`
				{ x + (_ * y) x } 3 4
			`,
			``,
			`15`,
		},
		{
			`
				(2+2 == 4 or "foo"+input == "foobar") and (2+2 == 5 or input == "bar")
			`,
			`bar`,
			`true`,
		},
		{
			`
				=a 3
				=b a + 12
				b + --a
			`,
			``,
			`12`,
		},
		{
			`
				=x "hi" + " there"
				=y " mis" + "ter"
				x + y
			`,
			``,
			`"hi there mister"`,
		},
	}
	for _, test := range tests {
		ast := parser.Parse(lexer.Tokenize(test.program))
		program, err := NewCompiler().Compile(ast)
		if err != nil {
			t.Fatal(err)
		}

		vm := NewVM()
		var output string
		vm.output = func(v Value) { output += v.String() }
		if err := vm.Run(program, test.input); err != nil {
			t.Fatal(err)
		} else if output != test.output {
			t.Errorf("expected %q, got %q", test.output, output)
		}
	}
}

func BenchmarkVM(b *testing.B) {
	vm := NewVM()
	vm.output = func(v Value) { b.Log(v) }
	vm.Run([]Instruction{
		instConstant{Value: ValInt(0)},
		instTarget{Name: "loop"},
		instConstant{Value: ValInt(1)},
		instInfixOp{Kind: token.Plus},
		instDup{},
		instJumpEq{Value: ValInt(b.N), Target: "done"},
		instJump{Target: "loop"},
		instTarget{Name: "done"},
		instOutput{},
	}, "")
}

func BenchmarkAST(b *testing.B) {
	p := parser.Parse(lexer.Tokenize(fmt.Sprintf(`
	replicate %v 1 | fold1 (+)
`, b.N)))
	e := evaluator.New()
	e.Run(p, "", func(v evaluator.Value) { b.Log(v) })
}
