package main

import (
	"io/ioutil"
	"os"

	"lukechampine.com/slouch/ast"
	"lukechampine.com/slouch/evaluator"
	"lukechampine.com/slouch/lexer"
	"lukechampine.com/slouch/parser"
)

func main() {
	prog := `
vvv
	var input = utils.Input(2020, 1)
	var inputInts = utils.ExtractInts(input)

	func part1() int {
		for _, i := range inputInts {
			for _, j := range inputInts {
				if i+j == 2020 {
					return i*j
				}
			}
		}
	}

	func part2() int {
		for _, i := range inputInts {
			for _, j := range inputInts {
				for _, k := range inputInts {
					if i+j+k == 2020 {
						return i*j*k
					}
				}
			}
		}
	}
^^^
part1
part2
`

	prog = `
=@ lines
choose 2 | first {x+y == 2020} | product
choose 3 | first {x+y+z == 2020} | product
`

	prog = `
choose 2 [1, 2, 3, 4] | map sum
`

	prog = `
=input ints
choose 2 input | first (sum | == 2020) | product
choose 3 input | first (sum | == 2020) | product
`

	/*
		TODO:

		- [ ] decide on lambda syntax
		- [ ] implement "argument holes"
			e.g. (choose _ input) should be a partial taking an int
			but map (_ + 3) input should NOT be a partial; just (_ + 3)
		- [x] reload on save
	*/

	input, err := ioutil.ReadFile(os.Args[1])
	if err != nil {
		panic(err)
	}
	ts := lexer.Tokenize(prog)
	//fmt.Println(ts)
	p := parser.Parse(ts)
	_ = ast.Print
	//fmt.Println(ast.Print(p))
	evaluator.New().Run(p, string(input))
}

/*

func main() {
	env := evaluator.New()
	prompt, _ := readline.New("∫> ")
	defer prompt.Close()
	prompt.Config.AutoComplete = env
	for {
		line, err := prompt.Readline()
		if err == io.EOF {
			break
		} else if err != nil {
			log.Fatal(err)
		}
		ts := lexer.Tokenize(line)
		p := parser.Parse(ts)
		eval(env, p.Expressions)

		for _, e := range env.Stack {
			fmt.Println(e)
		}
	}
}
*/
