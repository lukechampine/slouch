package main

import (
	"fmt"
	"io/ioutil"
	"os"
	"time"

	"github.com/pkg/profile"
	"lukechampine.com/slouch/ast"
	"lukechampine.com/slouch/evaluator"
	"lukechampine.com/slouch/lexer"
	"lukechampine.com/slouch/parser"
)

func main() {
	var prog string

	// 2020 day 1
	prog = `
=input ints
choose 2 input | first (sum | == 2020) | product
choose 3 input | first (sum | == 2020) | product
`

	// 2018 day 1
	prog = `
=input ints
sum
cycle | scan (+) | firstrepeat
`

	// 2015 day 2
	prog = `
=input lines | map ints
=sides choose 2 | map (product | (*2))
=wrap sides | {(sum x) + (min x)}
map wrap | sum
`

	// 2017 day 2
	prog = `
=input lines | map ints
map repeat | take 2 |< max - min | sum
=d sort | choose 2 | map reverse | first -<(%?) |< (/)
map d | sum
`

	// argument holes
	prog = `
3 | (_ * 4)
=fn (first _ [1,2,3])
fn (_ * 2 == 4)
[1,2] | sum == 3
`

	// function expr
	prog = `
(+1) 3
((_ * 2) 2) + ((*) 3 3)
`

	// splat
	prog = `
-< (+) [1,2] * 2
[1,2] |< * | +3
`

	// 2020 day 1
	prog = `
=input ints
choose 2 | first (sum == 2020) | product
choose 3 | first (sum == 2020) | product
`

	// 2016 day 3
	prog = `
=input lines | map ints
=validTri perms | all -<{x + y > z}
count validTri

=input transpose | concat | window 3
count validTri
`

	// 2016 day 4
	prog = `
=input lines | map (parse "%s-%d[%s]")
=input filter -< {
	delete "-" x |
	histogram |
	keys |
	take 5 |
	(== z)
}
map _.1 | sum

map -<{mapchar ((_-'a' + y % 26) + 'a') x}
`

	// 2016 day 5
	prog = `
=pws iota | map (input + str | md5) | filter (hasPrefix "00000")
take 8 pws | concatmap _.5
pws | filter (_.5 < 8) | take 8 | sortBy _.5 | concatmap _.7

`

	// 2017 day 4
	prog = `
=input lines | map words
count (dups | len == 0)
count (map sort | dups | len == 0)
`

	// 2016 day 6
	prog = `
=input lines | transpose | map histogram
map maxIndex | concat
map minIndex | concat
`

	// 2018 day 2
	prog = `
=input lines
=charcounts map histogram input
(count (hasVal 2) charcounts) * (count (hasVal 3) charcounts)
=sub1 head input | len - 1
choose 2 | map -<(same | concat) | first (len == sub1)
`

	// 2018 day 3
	prog = `
=input lines | map (ints | name ["id", "x", "y", "w", "h"])  ;;#123 @ 3,2: 5x4
=makerect rect @w @h | offset [@x, @y]
=g map makerect | concat | histogram
vals g | count (>1)
=alone makerect | all (g._ == 1)
first alone | @id
`

	// 2018 day 5
	prog = `
=units zipWith (+) alpha (toUpper alpha)
=units concat [units, (map reverse units)]
=react deleteAll units
=polymer stabilize react | len
polymer
=without deleteAll _ input | polymer
map without (zip alpha (toUpper alpha)) | min
`

	// 2019 day 1
	prog = `
=input ints
=fuel _ / 3 - 2
map fuel | sum
=recfuel iterate fuel | takeWhile (>0) | sum
map recfuel | sum
`

	// 2019 day 4
	prog = `
=input enum 284639 748759 | map digits
=dub runs | any (len >= 2)
count -:(sorted and dub)
=exactdub runs | any (len == 2)
count -:(sorted and exactdub)
`

	/*

		BUGS:
		- [ ] "iterator aliasing"
				=iot take _ iota
				[(iot 3), (iot 4), (iot 2)]
			* should return [[0, 1, 2], [0, 1, 2, 3], [0, 1]], instead returns [[0, 1, 2], [3, 4, 5, 6], [7, 8]]
				=iot take _ iota
				[(iot 3), (iot 4), (iot 2)] | first (len == 4)
			* should return [0, 1, 2, 3], instead returns []
			* in the general case, this requires analyzing the full program ahead of time...
			* for now, just make (deep!) copies everywhere
			* also need to duplicate iterator in partial fn args
			*
			* "garbage collection" -- use ref counts, and .clone when multiple refs exist
				* probably need to add .clone field to iterator

		PERF:
		- [ ] special case []int64 and []string for arrays (or iterators?)
		- [ ] track iterator size, for O(1) len and more efficient collect
		- [ ] panic(fmt.Sprintf("%T", v)) causes v to escape


		TODO:
		- [x] map literal syntax
		- [x] comments
		- [ ] lambdas
		- [ ] vscode/lsp
			* make a language server that handles highlighting, autocomplete, etc.
				* but also, onchange, re-run and send results to a "webview" extension

		IDEAS:
		- [x] "replicate" operator, for supplying one arg for all params
			e.g. -:(max - min)
		- [ ] assign to global within expr
			e.g. first -<(d=(diff) | len == 1) | delete d
		- [/] make pipe just another binop?
			* binops can have missing args, though; pipes can't
		- [/] paste problem description into AI; spits out possibly-relevant functions
			* probably better to have human-directed keyword search (e.g. "circular" spits out cycle)
		- [ ] time.Parse API for directions: parsedirs "U1 D2 L3 R4", parsedirs "N:1 S:2 W:3 E:4"
	*/

	input, err := ioutil.ReadFile(os.Args[1])
	if err != nil {
		panic(err)
	}
	ts := lexer.Tokenize(prog)
	p := parser.Parse(ts)
	_ = ast.Print
	//fmt.Println(ast.Print(p))
	start := time.Now()
	defer profile.Start(profile.CPUProfile, profile.ProfilePath(".")).Stop()
	evaluator.New().Run(p, string(input))
	out := fmt.Sprint("\nFinished in ", time.Since(start))
	_ = out
	fmt.Println(out)
}
