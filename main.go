package main

import (
	"fmt"
	"io"
	"log"

	"github.com/chzyer/readline"
	"lukechampine.com/slouch/ast"
	"lukechampine.com/slouch/evaluator"
	"lukechampine.com/slouch/lexer"
	"lukechampine.com/slouch/parser"
)

func eval(env *evaluator.Environment, exprs []ast.Expression) {
	stack := append([]evaluator.Value(nil), env.Stack...)
	defer func() {
		if r := recover(); r != nil {
			fmt.Println("Error:", r)
			env.Stack = stack
		}
	}()
	for _, e := range exprs {
		env.Eval(e)
	}
}

func main() {
	log.SetFlags(log.Lshortfile)
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
			fmt.Println("", e)
		}
	}
}
