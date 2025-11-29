package bytecode

import (
	"fmt"
	"strings"
	"testing"

	"lukechampine.com/slouch/evaluator"
	"lukechampine.com/slouch/lexer"
	"lukechampine.com/slouch/parser"
)

// TODO:
// - inline functions with jumps
// - tail call optimization??
// - statically determine arity of all functions

// replace icicles with streams when possible
//   need a test case demonstrating the bug...

// "type-checking" ("facts" map)

func TestCases(t *testing.T) {
	tests := []struct {
		program string
		input   string
		output  string
	}{
		{
			`
				[0 or 1, 1 or 0, 0 and 1, 1 and 0]
			`,
			``,
			`[true, true, false, false]`,
		},
		{
			`
				{[[1,x],[2,y]]} 3 4
			`,
			``,
			`[[1, 3], [2, 4]]`,
		},
		{
			`
				[3, 2] |< -
			`,
			``,
			`1`,
		},
		{
			`
				=foo {x - y - z} 3 2 1
				=bar (_ - _ - _ ) 3 2 1
				=baz (3 - _ - _ ) 2 1
				=bat (3 - 2 - _ ) 1
				=bax 3 - 2 - 1
				[foo, bar, baz, bat, bax]
			`,
			``,
			`[0, 0, 0, 0, 0]`,
		},
		{
			`
				=foo { y - x }
				=bar foo 2
				=baz foo 3
				bar 2 + baz 3
			`,
			``,
			`0`,
		},
		{
			`
				=foo -:{[[1,x],[2,y]] | map (.1) | -<(==)}
				foo input
			`,
			`"foobar"`,
			`true`,
		},
		{
			`
				=rec { len x == 0 or rec (tail x) }
				rec input
			`,
			`"foobar"`,
			`true`,
		},
		{
			`
				[1:"A", 2:"B", 3:"C"].2
			`,
			``,
			`"B"`,
		},
		{
			`
				2 | +3 | {[x,x]} |< %?
			`,
			``,
			`true`,
		},
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
		{
			`
				=isPalindrome { string x == (string x | reverse) }
				enum 100 999 | choose 2 | map product | filter isPalindrome | max
			`,
			``,
			`906609`,
		},
	}
	for _, test := range tests {
		ast := parser.Parse(lexer.Tokenize(test.program))
		c := NewCompiler()
		program, err := c.Compile(ast)
		if err != nil {
			t.Fatal(err)
		}
		// for _, diff := range c.optDiffs {
		// 	t.Logf("\n%v:\n%v", diff.desc, diff.patch)
		// }
		// fmt.Println("\nFinal program for " + test.program + ":\n" + program.String())

		vm := NewVM()
		var output string
		vm.output = func(v Value) { output += v.String() }
		if err := vm.Run(program, test.input); err != nil {
			t.Fatal(err)
		} else if output != test.output {
			t.Log(test.program)
			t.Log(program)
			t.Errorf("expected %q, got %q", test.output, output)
		}
	}
}

func TestComptime(t *testing.T) {
	tests := []struct {
		program string
		value   string
	}{
		{
			`
				toUpper "hello world"
			`,
			`"HELLO WORLD"`,
		},
		{
			`
				[0 or 1, 1 or 0, 0 and 1, 1 and 0]
			`,
			`[true, true, false, false]`,
		},
		{
			`
				{[[1,x],[2,y]]} 3 4
			`,
			`[[1, 3], [2, 4]]`,
		},
		{
			`
				[3, 2] |< -
			`,
			`1`,
		},
		{
			`
				=foo {x - y - z} 3 2 1
				=bar (_ - _ - _ ) 3 2 1
				=baz (3 - _ - _ ) 2 1
				=bat (3 - 2 - _ ) 1
				=bax 3 - 2 - 1
				[foo, bar, baz, bat, bax]
			`,
			`[0, 0, 0, 0, 0]`,
		},
		{
			`
				=foo { y - x }
				=bar foo 2
				=baz foo 3
				bar 2 + baz 3
			`,
			`0`,
		},
		{
			`
				[1:"A", 2:"B", 3:"C"].2
			`,
			`"B"`,
		},
		{
			`
				2 | +3 | {[x,x]} |< %?
			`,
			`true`,
		},
		{
			`
				{ x + (_ * y) x } 3 4
			`,
			`15`,
		},
		{
			`
				=a 3
				=b a + 12
				b + --a
			`,
			`12`,
		},
		{
			`
				=x "hi" + " there"
				=y " mis" + "ter"
				x + y
			`,
			`"hi there mister"`,
		},
	}
	for _, test := range tests {
		ast := parser.Parse(lexer.Tokenize(test.program))
		c := NewCompiler()
		program, err := c.Compile(ast)
		if err != nil {
			t.Fatal(err)
		}
		exp := strings.ReplaceAll(Program{
			instFuncDef{Name: "main"},
			instConstant{Value: ValInt(0)},
			instOutput{},
		}.String(), "0", test.value)
		if program.String() != exp {
			for _, diff := range c.optDiffs {
				t.Logf("\n%v:\n%v", diff.desc, diff.patch)
			}
			t.Errorf("program: %v\nexpected comptime value: %v\ngot:\n%v", test.program, test.value, program)
		}
	}
}

func BenchmarkVM(b *testing.B) {
	prog := fmt.Sprintf(`
	=isPalindrome { string x == (string x | reverse) }
	enum 0 %v | count isPalindrome
`, b.N)
	ast := parser.Parse(lexer.Tokenize(prog))
	program, err := NewCompiler().Compile(ast)
	if err != nil {
		b.Fatal(err)
	}

	vm := NewVM()
	vm.output = func(v Value) { b.Log(b.N, v) }

	b.ResetTimer()
	vm.Run(program, "")
}

func BenchmarkAST(b *testing.B) {
	p := parser.Parse(lexer.Tokenize(fmt.Sprintf(`
	=isPalindrome { string x == (string x | reverse) }
	enum 0 %v | count isPalindrome
`, b.N)))
	e := evaluator.New()

	b.ResetTimer()
	e.Run(p, "", func(v evaluator.Value) { b.Log(b.N, v) })
}
