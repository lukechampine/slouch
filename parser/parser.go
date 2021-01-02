// Package parser ...
package parser

import (
	"fmt"

	"lukechampine.com/slouch/ast"
	"lukechampine.com/slouch/token"
)

const (
	precLowest = iota
	precPipe
	precOr
	precAnd
	precNot
	precAssign
	precEquals
	precCmp
	precPlus
	precProd
	precNeg
	precCall
	precIndex
)

var precedences = map[token.Kind]int{
	token.Ident: precCall,
	token.Pipe:  precPipe,
	// token.OR:     OR,
	// token.AND:    AND,
	// token.NOT:    NOT,
	// token.BIND:   ASSIGN,
	// token.ASSIGN: ASSIGN,
	token.Equals: precEquals,
	// token.NEQ:    EQUALS,
	// token.LT:     LESSGREATER,
	// token.GT:     LESSGREATER,
	// token.LTE:    LESSGREATER,
	// token.GTE:    LESSGREATER,

	token.Plus: precPlus,
	// token.MINUS:    SUM,
	// token.DIVIDE:   PRODUCT,
	// token.MULTIPLY: PRODUCT,
	// token.MODULO:   PRODUCT,
	// token.LPAREN:   CALL,
	// token.LBRACKET: INDEX,
	// token.DOT:      INDEX,
}

// Parse parses a sequence of tokens as a program.
func Parse(ts []token.Token) ast.Program {
	return newParser(ts).parseProgram()
}

// A Parser parses slouch programs.
type Parser struct {
	ts []token.Token

	prefixFns map[token.Kind]func() ast.Expr
	infixFns  map[token.Kind]func(ast.Expr) ast.Expr
}

func newParser(ts []token.Token) *Parser {
	p := &Parser{
		ts: ts,
	}
	p.prefixFns = map[token.Kind]func() ast.Expr{
		token.Ident:  p.parseIdentifier,
		token.Int:    p.parseInteger,
		token.String: p.parseString,
		//token.Bang:    p.parsePrefixOp,
		token.Lbrace: p.parseArray,
		token.Lparen: p.parseParens,
		token.Plus:   p.parsePrefixInfixOp,
		token.Star:   p.parsePrefixInfixOp,
		token.Equals: p.parsePrefixInfixOp,
	}
	p.infixFns = map[token.Kind]func(ast.Expr) ast.Expr{
		token.Pipe:   p.parsePipe,
		token.Plus:   p.parseInfixOp,
		token.Star:   p.parseInfixOp,
		token.Equals: p.parseInfixOp,
		//token.PipeEquals: p.parseAssignment,
		//token.Minus:      p.parseInfixOp,
	}
	return p
}

func (p *Parser) cur() token.Token {
	if len(p.ts) == 0 {
		return token.Token{Kind: token.EOF}
	}
	return p.ts[0]
}

func (p *Parser) peek() token.Token {
	if len(p.ts) < 2 {
		return token.Token{Kind: token.EOF}
	}
	return p.ts[1]
}

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

func (p *Parser) advance() {
	if len(p.ts) > 0 {
		p.ts = p.ts[1:]
	}
}

func (p *Parser) expect(ks ...token.Kind) {
	if len(p.ts) == 0 {
		panic(fmt.Sprintf("expected one of %v, got EOF", ks))
	}
	for _, k := range ks {
		if p.cur().Kind == k {
			return
		}
	}
	panic(fmt.Sprintf("expected one of %v, got %v", ks, p.cur()))
}

func (p *Parser) consume(k token.Kind) token.Token {
	p.expect(k)
	t := p.cur()
	p.advance()
	return t
}

func (p *Parser) consumeWhitespace() {
	for p.cur().Kind == token.Newline {
		p.advance()
	}
}

func (p *Parser) parseProgram() ast.Program {
	var prog ast.Program
	for p.cur().Kind != token.EOF {
		prog.Stmts = append(prog.Stmts, p.parseStmt())
	}
	return prog
}

func (p *Parser) parseStmt() (s ast.Stmt) {
	p.consumeWhitespace()
	switch t := p.cur(); t.Kind {
	case token.Assign:
		s = p.parseAssignStmt()
	case token.Snippet:
		s = ast.SnippetStmt{Token: t, Code: t.Lit}
	default:
		// any other token indicates an expression statement
		s = ast.ExprStmt{X: p.parseExpr(precLowest)}
	}
	// all statements should end in a newline
	p.advance()
	p.consume(token.Newline)
	return
}

func (p *Parser) parseAssignStmt() ast.Stmt {
	p.consume(token.Assign)
	name := p.consume(token.Ident)
	return ast.AssignStmt{
		Name: ast.Ident{Token: name, Name: name.Lit},
		X:    p.parseExpr(precLowest),
	}
}

func (p *Parser) parseExpr(prec int) ast.Expr {
	t := p.cur()
	pfn, ok := p.prefixFns[t.Kind]
	if !ok {
		panic(fmt.Sprintln("no prefix for", t.Kind))
	}
	e := pfn()

	for !p.peekIs(token.Newline, token.EOF) && prec < precedences[p.peek().Kind] {
		ifn, ok := p.infixFns[p.peek().Kind]
		if !ok {
			break
		}
		p.advance()
		e = ifn(e)
	}
	return e
}

func (p *Parser) parseInteger() ast.Expr {
	t := p.cur()
	return ast.Integer{Token: t, Value: t.Lit}
}

func (p *Parser) parseString() ast.Expr {
	t := p.cur()
	return ast.String{Token: t, Value: t.Lit}
}

func (p *Parser) parseIdentifier() ast.Expr {
	t := p.cur()
	i := ast.Ident{Token: t, Name: t.Lit}

	// if there are args, this is a fn call
	var args []ast.Expr
	for p.peekIs(token.Ident, token.Int, token.String, token.Lbrace, token.Lparen) {
		p.advance()
		args = append(args, p.parseExpr(precCall))
	}
	if len(args) > 0 {
		return ast.FnCall{
			Token: t,
			Fn:    i,
			Args:  args,
		}
	}
	return i
}

func (p *Parser) parseArray() ast.Expr {
	p.advance() // advance past [
	var a ast.Array
	for !p.curIs(token.Rbrace, token.EOF) {
		e := p.parseExpr(precLowest)
		a.Elems = append(a.Elems, e)
		p.advance()
		if !p.curIs(token.Comma) {
			break
		}
		p.advance()
	}
	p.expect(token.Rbrace)
	return a
}

func (p *Parser) parseParens() ast.Expr {
	p.advance() // advance past (
	e := p.parseExpr(precLowest)
	p.advance() // advance to )
	return e
}

/*
func (p *Parser) parsePrefixOp() ast.Expr {
	t := p.cur()
	e := ast.PrefixOp{
		Token:    t,
		Operator: t.Kind,
	}
	p.advance()
	e.Right = p.parseExpr(precPrefix)
	return e
}


*/

func (p *Parser) parsePrefixInfixOp() ast.Expr {
	t := p.cur()
	e := ast.InfixOp{
		Token: t,
		Op:    t.Kind,
		Left:  nil,
	}
	if !p.peekIs(token.Rparen) {
		p.advance()
		e.Right = p.parseExpr(precLowest) // TODO: wrong?
	}
	return e
}

func (p *Parser) parseInfixOp(left ast.Expr) ast.Expr {
	t := p.cur()
	e := ast.InfixOp{
		Token: t, // TODO: wrong?
		Op:    t.Kind,
		Left:  left,
	}
	p.advance()
	e.Right = p.parseExpr(precedences[t.Kind])
	return e
}

func (p *Parser) parsePipe(left ast.Expr) ast.Expr {
	t := p.cur()
	e := ast.Pipe{
		Token: t,
		Left:  left,
	}
	p.advance()
	e.Right = p.parseExpr(precedences[t.Kind])
	return e
}

/*
func (p *Parser) parseExpr(prec int) ast.Expr {
	p.consumeWhitespace()
	switch t := p.cur(); t.Kind {
	case token.Lbrace:
		var a ast.Array
		p.advance()
		for len(p.ts) > 0 && p.cur().Kind != token.Rbrace {
			a.Elems = append(a.Elems, p.parseExpr())
			p.advance()
			p.expect(token.Comma, token.Rbrace)
			if p.cur().Kind == token.Comma {
				p.advance()
			}
		}
		return a
	case token.Lparen:
		p.advance()
		e := p.parseExpr()
		p.advance()
		p.expect(token.Rparen)
		return e
	default:
		panic(t)
	}
}

func (p *Parser) parseFnArgs() []ast.Expr {
	var args []ast.Expr
	for {
		switch p.peek().Kind {
		case token.EOF, token.Newline, token.Rparen, token.Rbrace, token.Pipe, token.Buck: // TODO: probably more
			return args
		}
		p.advance()
		args = append(args, p.parseExpr())
	}
}
*/
