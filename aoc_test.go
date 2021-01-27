package main

import (
	"fmt"
	"io/ioutil"
	"reflect"
	"strings"
	"testing"
	"time"

	"github.com/fatih/color"
	"lukechampine.com/slouch/evaluator"
	"lukechampine.com/slouch/lexer"
	"lukechampine.com/slouch/parser"
)

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
			year: 2015, day: 5,
			prog: `
=input lines

=hasVowels chars | count (contains _ "aeiou") >= 3
=hasDouble chars | window 2 | any -<(==)
=noBlacklist containsAny ["ab", "cd", "pq", "xy"] | not
=nice -: hasVowels and hasDouble and noBlacklist
count nice

=hasPair { window 2 x | any (count _ x >= 2) }
=hasABA chars | window 3 | any -<{ x == z }
=nice -: hasPair and hasABA
count nice
`,
			exp: `
255
55
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
=d choose 2 | map (sortBy (>)) | first -<(%?) |< (/)
map d | sum
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
			year: 2018, day: 1,
			prog: `
=input ints
sum
cycle | scan (+) | firstrepeat
`,
			exp: `
518
72889
`,
		},
		{
			year: 2018, day: 2,
			prog: `
=input lines
map histogram input |: (count (hasVal 2)) * (count (hasVal 3))
=sub1 head input | len - 1
choose 2 | map -<(same) | first (len == sub1) | concat
`,
			exp: `
7657
ivjhcadokeltwgsfsmqwrbnuy
`,
		},
		{
			year: 2018, day: 5,
			prog: `
=units zipWith (+) alpha (toUpper alpha)
=units concat [units, (map reverse units)]
=react deleteAll units
=polymer stabilize react | len
polymer input
=without deleteAll _ input | polymer
map without (zip alpha (toUpper alpha)) | min
`,
			exp: `
9822
5726
`,
		},

		{
			year: 2019, day: 1,
			prog: `
=input ints
=fuel {x/3 - 2}
map fuel | sum
=recfuel iterate fuel | takeWhile (>0) | sum
map recfuel | sum
`,
			exp: `
3173518
4757427
`,
		},
		{
			year: 2019, day: 4,
			prog: `
=input split "-" | map int |< enum | map digits | filter sorted
count (runs | any (len >= 2))
count (runs | any (len == 2))
`,
			exp: `
895
591
`,
		},
		{
			year: 2020, day: 1,
			prog: `
=input ints
choose 2 | first (sum == 2020) | product
choose 3 | first (sum == 2020) | product
`,
			exp: `
744475
70276940
`,
		},
		{
			year: 2020, day: 6,
			prog: `
=input split "\n\n" | map lines
map (concat | histogram | len) | sum
map { count (len x) (concat x | histogram | vals) } | sum
`,
			exp: `
7027
3579
`,
		},
	}
	for _, test := range tests {
		input, err := ioutil.ReadFile(fmt.Sprintf("inputs/%v_day%v.txt", test.year, test.day))
		if err != nil {
			t.Error("missing input for", test.year, test.day)
			continue
		}
		p := parser.Parse(lexer.Tokenize(test.prog))
		var output []string
		start := time.Now()
		err = evaluator.New().Run(p, string(input), func(v evaluator.Value) {
			output = append(output, v.String())
		})
		elapsed := time.Since(start)
		if err != nil {
			t.Fatal(err)
		}
		if exp := strings.Split(strings.TrimSpace(test.exp), "\n"); !reflect.DeepEqual(output, exp) {
			t.Errorf("output did not match for %v day%v:\nexp:\n%v\ngot:\n%v", test.year, test.day,
				strings.Join(exp, "\n"), strings.Join(output, "\n"))
			continue
		}
		elapsedStr := elapsed.String()
		switch {
		case elapsed < 100*time.Millisecond:
			elapsedStr = color.GreenString(elapsedStr)
		case elapsed < 500*time.Millisecond:
			elapsedStr = color.WhiteString(elapsedStr)
		case elapsed < 1*time.Second:
			elapsedStr = color.YellowString(elapsedStr)
		default:
			elapsedStr = color.RedString(elapsedStr)
		}
		t.Logf("%v day %v PASS (%v)", test.year, test.day, elapsedStr)
	}
}
