package main

import (
	"bufio"
	"fmt"
	"log"
	"os"

	"lukechampine.com/slouch/ast"
	"lukechampine.com/slouch/evaluator"
	"lukechampine.com/slouch/lexer"
	"lukechampine.com/slouch/parser"
)

func main() {
	log.SetFlags(log.Lshortfile)
	env := evaluator.New()
	scanner := bufio.NewScanner(os.Stdin)
	fmt.Printf("∫> ")
	for scanner.Scan() {
		line := scanner.Text()
		ts := lexer.Tokenize(line)
		fmt.Println("tokens: ", ts)
		p := parser.Parse(ts)
		fmt.Println("parse:\n", ast.Print(p))
		for _, e := range p.Expressions {
			fmt.Println(env.Eval(e))
		}
		fmt.Printf("∫> ")
	}
	fmt.Println()
}
