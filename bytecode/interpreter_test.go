package bytecode

import (
	"fmt"
	"os"
	"strings"
	"testing"
	"time"

	"lukechampine.com/slouch/evaluator"
	"lukechampine.com/slouch/lexer"
	"lukechampine.com/slouch/parser"
)

// TODO:
// - inline functions with jumps
// - tail call optimization??
// - statically determine arity of all functions
// - compute budget; pause/resume; watch values returned by each function
//   - builtins tho...
//
// - renaming builtins, i.e. =map (+2)
//   - currently would cause issues with passStreamFusion, maybe others

// replace icicles with streams when possible
//   need a test case demonstrating the bug...

func TestCases(t *testing.T) {
	tests := []struct {
		program string
		input   string
		output  string
	}{

		{
			`
				[1, 2, input] | map (+"bar") | .2
			`,
			`"foo"`,
			`"foobar"`,
		},
		{
			`
				=rec { len x and rec2 (tail x) }
				=rec2 { len x and rec (tail x) }
				=bar { rec [x, x] }
				=baz { rec2 [x, x, x] }
				bar 2 and baz input
			`,
			`"foobar"`,
			`false`,
		},
		{
			`
				=a "foo"
				=a "bar"
				a
			`,
			``,
			`"bar"`,
		},
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
				=rec { len x and rec2 (tail x) }
				=rec2 { len x and rec (tail x) }
				=bar { rec [x, x] }
				=baz { rec2 [x, x, x] }
				bar 2 and baz input
			`,
			`"foobar"`,
			`false`,
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
			`"bar"`,
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
		var input Value
		if test.input != "" {
			input, err = ParseValue(parser.ParseExpr(lexer.Tokenize(test.input)))
			if err != nil {
				t.Fatal(err)
			}
		}
		var output string
		vm := NewVM(func(v Value) { output += v.String() })
		if err := vm.Run(program, input); err != nil {
			t.Fatal(err)
		} else if output != test.output {
			for _, diff := range c.optDiffs {
				t.Logf("\n%v:\n%v", diff.desc, diff.patch)
			}
			t.Log("\nFinal program for " + test.program + ":\n" + program.String())
			t.Fatalf("expected %q, got %q", test.output, output)
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
				["foo", 2, true] | map (+2) | .1
			`,
			`4`,
		},
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
		{
			`
				=x "foobar" + input
				=y "foo" + "bar" + input
				=z "f" + "o" + "o" + "b" + "a" + "r" + input
				x == y and y == z
			`,
			`true`,
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

	vm := NewVM(func(v Value) { b.Log(b.N, v) })
	b.ResetTimer()
	vm.Run(program, nil)
}

func BenchmarkAST(b *testing.B) {
	p := parser.Parse(lexer.Tokenize(fmt.Sprintf(`
	enum 0 %v | map { x / 2 + x * 3 - x %% 4 } | sum
`, b.N)))
	e := evaluator.New()

	b.ResetTimer()
	e.Run(p, "", func(v evaluator.Value) { b.Log(b.N, v) })
}

func TestAoC(t *testing.T) {
	tests := []struct {
		year, day int
		prog      string
		exp       string
	}{

		{
			year: 2015, day: 1,
			prog: `
					=input map (["(":1, ")":-1].)
					sum
					scan (+) | takeWhile (>=0) | len + 1
				 `,
			exp: `
					232
					1783
				 `,
		},
		{
			year: 2015, day: 2,
			prog: `
					=input lines | map ints
					=wrap choose 2 | map product |: sum*2 + min
					map wrap | sum
					=ribbon sort | take 2 | sum * 2
					=bow product
					map -:(ribbon + bow) | sum
				 `,
			exp: `
					1588178
					3783758
				 `,
		},
		{

			year: 2016, day: 3,
			prog: `
					=input lines | map ints
					=validTri perms | all -<{x + y > z}
					count validTri
					=input transpose | concat | partition 3
					count validTri
				 `,
			exp: `
					982
					1826
				 `,
		},
		{
			year: 2016, day: 6,
			prog: `
					=input lines | transpose | map histogram
					map maxIndex | concat
					map minIndex | concat
				 `,
			exp: `
					nabgqlcw
					ovtrjcjh
				 `,
		},
		{
			year: 2017, day: 2,
			prog: `
					=input lines | map ints
					map -:(max - min) | sum
					sumWith (choose 2 | map (sortBy (>)) | first -<(%?) |< (/))
				 `,
			exp: `
					34925
					221
				 `,
		},
		{
			year: 2017, day: 4,
			prog: `
					=input lines | map words
					count (dups | len == 0)
					count (map sort | dups | len == 0)
				 `,
			exp: `
					466
					251
				 `,
		},
		{
			year: 2019, day: 1,
			prog: `
					=input ints
					=fuel {x/3 - 2}
					map fuel | sum
					=recfuel fuel | iterate fuel | takeWhile (>0) | sum
					map recfuel | sum
				 `,
			exp: `
					3173518
					4757427
				 `,
		},
		{
			year: 2020, day: 1,
			prog: `
					input | ints | choose 3 | first (sum == 2020) | product
				 `,
			exp: `
					70276940
				 `,
		},
	}

	for i, test := range tests {
		if i != len(tests)-1 {
			continue
		}
		input, err := os.ReadFile(fmt.Sprintf("../inputs/%v_day%v.txt", test.year, test.day))
		if err != nil {
			t.Error("missing input for", test.year, test.day)
			continue
		}

		start := time.Now()
		c := NewCompiler()
		program, err := c.Compile(parser.Parse(lexer.Tokenize(test.prog)))
		if err != nil {
			t.Fatal(err)
		}
		compileTime := time.Since(start)

		for _, diff := range c.optDiffs {
			t.Logf("\n%v:\n%v", diff.desc, diff.patch)
		}
		t.Log("\nFinal program for " + test.prog + ":\n" + program.String())

		var outputLines []string
		vm := NewVM(func(v Value) { outputLines = append(outputLines, v.String()) })
		start = time.Now()
		if err := vm.Run(program, ValString(input)); err != nil {
			t.Fatal(err)
		}
		elapsed := time.Since(start)
		output := strings.Join(outputLines, "\n")
		exp := strings.TrimSpace(strings.ReplaceAll(test.exp, "\t", ""))
		if output != exp {
			for _, diff := range c.optDiffs {
				t.Logf("\n%v:\n%v", diff.desc, diff.patch)
			}
			t.Log("\nFinal program for " + test.prog + ":\n" + program.String())
			t.Errorf("output did not match for %v day%v:\nexp:\n%q\ngot:\n%q", test.year, test.day,
				exp, output)
			continue
		}
		t.Logf("%v day %2v PASS (%2ds %3dms, compile: %2ds %3dms)", test.year, test.day,
			elapsed/time.Second, (elapsed%time.Second)/time.Millisecond,
			compileTime/time.Second, (compileTime%time.Second)/time.Millisecond)
	}
}
