package main

import (
	"fmt"
	"io/ioutil"
	"os"
	"time"

	"lukechampine.com/slouch/evaluator"
	"lukechampine.com/slouch/lexer"
	"lukechampine.com/slouch/parser"
)

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
	* add "clone" field to iterator?

	PERF:
	- [ ] special case []int64 and []string for arrays (or iterators?)
	- [ ] track iterator size, for O(1) len and more efficient collect
		* not possible for e.g. iota | takeWhile (<10)
	- [ ] panic(fmt.Sprintf("%T", v)) causes v to escape
	- [ ] refcount all values; reuse memory if 0
	- [ ] memoization rune or builtin
		* e.g. memo (last x) only computes (last x) once

	TODO:
	- [x] proper test framework
	- [x] start tracking how many AoC problems have (complete, working, idiomatic, performant) solutions
	- [ ] cleanup Builtin/Partial distinction; should just have one FuncValue type
	- [ ] reconsider automatically converting strings to iterator
		* chars builtin is probably less surprising
	- [ ] make _ work in arrays
	- [ ] vscode/lsp
		* make a language server that handles highlighting, autocomplete, etc.
			* but also, onchange, re-run and send results to a "webview" extension

	IDEAS:
	- [ ] make "truthy" explicit, never implicit
	- [ ] assign to global within expr
		e.g. first -<(d=(diff) | len == 1) | delete d
	- [/] paste problem description into AI; spits out possibly-relevant functions
		* probably better to have human-directed keyword search (e.g. "circular" spits out cycle)
	- [ ] time.Parse-style API for directions: parsedirs "U1 D2 L3 R4", parsedirs "N:1 S:2 W:3 E:4"
*/

func main() {
	var prog string

	// 2018 day 3
	prog = `
=input lines | map (ints | name ["id", "x", "y", "w", "h"])  ;;#123 @ 3,2: 5x4
=makerect rect @w @h | offset [@x, @y]
=g map makerect | concat | histogram
vals g | count (>1)
=alone makerect | all (g._ == 1)
first alone | @id
`

	// 2020 day 3
	prog = `
;=input boolgrid "#."
;iterate (offset [3,1]) [0,0] | takeWhile (inbounds input) | map (input._) | sum
`

	// 2020 day 10
	prog = `
=input ints
sort | deltas | histogram |: (_.1 + 1) * (_.3 + 1)
`

	input, err := ioutil.ReadFile(os.Args[1])
	if err != nil {
		panic(err)
	}
	p := parser.Parse(lexer.Tokenize(prog))
	start := time.Now()
	err = evaluator.New().Run(p, string(input), func(v evaluator.Value) { fmt.Println(v) })
	if err != nil {
		panic(err)
	}
	fmt.Println("\nFinished in", time.Since(start))
}
