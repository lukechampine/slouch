// Package parser ...
package parser

import (
	"fmt"
	"log"

	"lukechampine.com/slouch/ast"
	"lukechampine.com/slouch/token"
)

const (
	precLowest = iota
	precEquals // ==
	precCmp    // > or <
	precAssign // |=
	precLambda // |
	precPipe   // |
	precCall   // f x
	precPlus   // + or -
	precMul    // *
	precPrefix // -X or !X
)

var precedences = map[token.Kind]int{
	token.Pipe:       precPipe,
	token.PipeEquals: precAssign,
	token.Plus:       precPlus,
	token.Minus:      precPlus,
}

func Parse(ts []token.Token) ast.Program {
	return New(ts).ParseProgram()
}

type Parser struct {
	ts []token.Token

	prefixFns map[token.Kind]func() ast.Expression
	infixFns  map[token.Kind]func(ast.Expression) ast.Expression
}

func New(ts []token.Token) *Parser {
	p := &Parser{
		ts: ts,
	}
	p.prefixFns = map[token.Kind]func() ast.Expression{
		token.Ident:   p.parseFunctionCall,
		token.Builtin: p.parseFunctionCall,
		token.Int:     p.parseInteger,
		token.String:  p.parseString,
		token.Bang:    p.parsePrefixOp,
		token.Lbrace:  p.parseArray,
		token.Lparen:  p.parseParens,
		token.Pipe:    p.parseLambda,
	}
	p.infixFns = map[token.Kind]func(ast.Expression) ast.Expression{
		token.Pipe:       p.parsePipe,
		token.PipeEquals: p.parseAssignment,
		token.Plus:       p.parseInfixOp,
		token.Minus:      p.parseInfixOp,
	}
	return p
}

func (p *Parser) advance() {
	if len(p.ts) > 0 {
		p.ts = p.ts[1:]
	}
}
func (p *Parser) cur() token.Token  { return p.ts[0] }
func (p *Parser) peek() token.Token { return p.ts[1] }
func (p *Parser) canPeek() bool     { return len(p.ts) > 1 }

func (p *Parser) curIs(kinds ...token.Kind) bool {
	t := p.cur()
	for _, k := range kinds {
		if t.Kind == k {
			return true
		}
	}
	return false
}

func (p *Parser) peekIs(kinds ...token.Kind) bool {
	t := p.peek()
	for _, k := range kinds {
		if t.Kind == k {
			return true
		}
	}
	return false
}

func (p *Parser) ParseProgram() ast.Program {
	var prog ast.Program
	for len(p.ts) > 0 {
		e := p.parseExpression(precLowest)
		prog.Expressions = append(prog.Expressions, e)
		p.advance() // advance to newline
		if len(p.ts) > 0 && !p.curIs(token.Newline) {
			log.Fatalln("expected newline")
		}
		p.advance() // skip newline
	}
	return prog
}

func (p *Parser) parseExpression(prec int) ast.Expression {
	t := p.cur()
	pfn, ok := p.prefixFns[t.Kind]
	if !ok {
		panic(fmt.Sprintln("no prefix for", t.Kind))
	}
	e := pfn()

	for p.canPeek() && !p.peekIs(token.Newline) && prec < precedences[p.peek().Kind] {
		ifn, ok := p.infixFns[p.peek().Kind]
		if !ok {
			break
		}
		p.advance()
		e = ifn(e)
	}
	return e
}

func (p *Parser) parseIdentifier() ast.Expression {
	t := p.cur()
	return ast.Identifier{Token: t, Name: t.Lit}
}

func (p *Parser) parseBuiltin() ast.Expression {
	t := p.cur()
	return ast.Builtin{Token: t, Name: t.Lit}
}

func (p *Parser) parseFunctionCall() ast.Expression {
	t := p.cur()
	fc := ast.FunctionCall{Token: t}
	switch t.Kind {
	case token.Builtin:
		fc.Fn = p.parseBuiltin()
	case token.Ident:
		fc.Fn = p.parseIdentifier()
	default:
		panic("illegal fn call token")
	}
	// consume until hitting a terminal (infixOp, pipe, paren, newline, EOF)
	for p.canPeek() && !p.peekIs(token.Plus, token.Minus, token.Pipe, token.PipeEquals, token.Rparen, token.Newline) {
		p.advance()
		a := p.parseExpression(precCall)
		fc.Args = append(fc.Args, a)
	}
	return fc
}

func (p *Parser) parseLambda() ast.Expression {
	t := p.cur()
	l := ast.Lambda{Token: t}
	p.advance() // past |
	l.Body = p.parseExpression(precLambda)
	return l
}

func (p *Parser) parseInteger() ast.Expression {
	t := p.cur()
	return ast.Integer{Token: t, Value: t.Lit}
}

func (p *Parser) parseString() ast.Expression {
	t := p.cur()
	return ast.String{Token: t, Value: t.Lit}
}

func (p *Parser) parseArray() ast.Expression {
	p.advance() // advance past [
	var a ast.Array
	for p.cur().Kind != token.Rbrace {
		e := p.parseExpression(precLowest)
		a.Elements = append(a.Elements, e)
		p.advance()
	}
	return a
}

func (p *Parser) parseParens() ast.Expression {
	p.advance() // advance past (
	e := p.parseExpression(precLowest)
	p.advance() // advance to )
	return e
}

func (p *Parser) parsePrefixOp() ast.Expression {
	t := p.cur()
	e := ast.PrefixOp{
		Token:    t,
		Operator: t.Kind,
	}
	p.advance()
	e.Right = p.parseExpression(precPrefix)
	return e
}

func (p *Parser) parseInfixOp(left ast.Expression) ast.Expression {
	t := p.cur()
	e := ast.InfixOp{
		Token:    t,
		Left:     left,
		Operator: t.Kind,
	}
	p.advance()
	e.Right = p.parseExpression(precedences[t.Kind])
	return e
}

func (p *Parser) parsePipe(left ast.Expression) ast.Expression {
	t := p.cur()
	e := ast.Pipe{
		Token: t,
		Left:  left,
	}
	p.advance()
	e.Right = p.parseExpression(precedences[t.Kind])
	return e
}

func (p *Parser) parseAssignment(left ast.Expression) ast.Expression {
	t := p.cur()
	e := ast.Assignment{
		Token: t,
		Value: left,
	}
	p.advance()
	e.Var = p.parseIdentifier().(ast.Identifier)
	return e
}
