// Package parser ...
package parser

import (
	"lukechampine.com/slouch/ast"
	"lukechampine.com/slouch/token"
)

const (
	precLowest = iota
)

var precedences = map[token.Kind]int{}

func Parse(ts []token.Token) ast.Program {
	return New(ts).ParseProgram()
}

type Parser struct {
	ts []token.Token
}

func New(ts []token.Token) *Parser {
	return &Parser{
		ts: ts,
	}
}

func (p *Parser) advance() {
	if len(p.ts) > 0 {
		p.ts = p.ts[1:]
	}
}

func (p *Parser) ParseProgram() ast.Program {
	var prog ast.Program
	for len(p.ts) > 0 {
		prog.Expressions = append(prog.Expressions, p.parseExpression())
		p.advance()
	}
	return prog
}

func (p *Parser) parseExpression() ast.Expression {
	t := p.ts[0]
	switch t.Kind {
	case token.Int:
		return ast.Integer{Token: t, Value: t.Lit}
	case token.String:
		return ast.String{Token: t, Value: t.Lit}
	case token.Ident:
		return ast.Identifier{Token: t, Name: t.Lit}
	case token.Lparen:
		var q ast.Quotation
		p.advance()
		for len(p.ts) > 0 && p.ts[0].Kind != token.Rparen {
			q.Body = append(q.Body, p.parseExpression())
			p.advance()
		}
		return q
	default:
		panic(t)
	}
}
