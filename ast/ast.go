// Package ast ...
package ast

import (
	"fmt"
	"strconv"
	"strings"

	"lukechampine.com/slouch/token"
)

type Node interface {
	fmt.Stringer
	isNode()
}

type Expr interface {
	Node
	isExpr()
}

type Stmt interface {
	Node
	isStmt()
}

type Ident struct {
	Token token.Token // the IDENT token
	Name  string
}

func (i Ident) isNode()        {}
func (i Ident) isExpr()        {}
func (i Ident) String() string { return i.Name }

// e.g. 123
type Integer struct {
	Token token.Token
	Value string
}

func (i Integer) isNode()        {}
func (i Integer) isExpr()        {}
func (i Integer) String() string { return i.Value }

// e.g. "123"
type String struct {
	Token token.Token
	Value string
}

func (s String) isNode()        {}
func (s String) isExpr()        {}
func (s String) String() string { return strconv.Quote(s.Value) }

// e.g. [ 1 2 3 ]
type Array struct {
	Token token.Token
	Elems []Expr
}

func (a Array) isNode() {}
func (a Array) isExpr() {}
func (a Array) String() string {
	strs := make([]string, len(a.Elems))
	for i, t := range a.Elems {
		strs[i] = t.String()
	}
	return "[ " + strings.Join(strs, " ") + " ]"
}

// e.g. 2+2
type InfixOp struct {
	Token       token.Token
	Op          token.Kind
	Left, Right Expr
}

func (i InfixOp) isNode()        {}
func (i InfixOp) isExpr()        {}
func (i InfixOp) String() string { return i.Left.String() + " " + i.Token.Lit + " " + i.Right.String() }

// e.g. { x * 2 }
type Lambda struct {
	Token token.Token
	Body  Expr
}

func (l Lambda) isNode()        {}
func (l Lambda) isExpr()        {}
func (l Lambda) String() string { return "{ " + l.Body.String() + " }" }

// e.g. foo 1 2 3
type FnCall struct {
	Token token.Token
	Fn    Expr
	Args  []Expr
}

func (c FnCall) isNode() {}
func (c FnCall) isExpr() {}
func (c FnCall) String() string {
	strs := make([]string, 1+len(c.Args))
	strs[0] = c.Fn.String()
	for i, t := range c.Args {
		strs[i+1] = t.String()
	}
	return strings.Join(strs, " ")
}

// e.g. foo | bar
//
// TODO: just another infixOp?
type Pipe struct {
	Token       token.Token
	Left, Right Expr
}

func (p Pipe) isNode()        {}
func (p Pipe) isExpr()        {}
func (p Pipe) String() string { return p.Left.String() + " | " + p.Right.String() }

// e.g. foo 1 2 3
type ExprStmt struct {
	X Expr
}

func (s ExprStmt) isNode()        {}
func (s ExprStmt) isStmt()        {}
func (s ExprStmt) String() string { return s.X.String() }

// e.g. =x foo 1 2 3
type AssignStmt struct {
	Token token.Token
	Name  Ident
	X     Expr
}

func (s AssignStmt) isNode()        {}
func (s AssignStmt) isStmt()        {}
func (s AssignStmt) String() string { return "=" + s.Name.String() + " " + s.X.String() }

// e.g. vvv println("foo") ^^^
type SnippetStmt struct {
	Token token.Token
	Code  string
}

func (s SnippetStmt) isNode()        {}
func (s SnippetStmt) isStmt()        {}
func (s SnippetStmt) String() string { return "vvv\n" + s.Code + "\n^^^" }

type Program struct {
	Stmts []Stmt
}

func (p Program) isNode() {}
func (p Program) String() string {
	strs := make([]string, len(p.Stmts))
	for i := range strs {
		strs[i] = p.Stmts[i].String()
	}
	return strings.Join(strs, "\n")
}

func Print(n Node) string {
	var b strings.Builder
	recPrint(&b, 0, n)
	return b.String()
}

func recPrint(b *strings.Builder, indent int, n Node) {
	writeLine := func(s string) {
		for i := 0; i < indent; i++ {
			b.WriteString("  ")
		}
		b.WriteString(s)
		b.WriteByte('\n')
	}
	switch n := n.(type) {
	case Program:
		writeLine("PROGRAM:")
		for _, e := range n.Stmts {
			recPrint(b, indent+1, e)
		}
	case Integer:
		writeLine("INTEGER: " + n.String())
	case String:
		writeLine("STRING: " + n.String())
	case Ident:
		writeLine("IDENT: " + n.String())
	case Array:
		writeLine("ARRAY:")
		for _, e := range n.Elems {
			recPrint(b, indent+1, e)
		}
	case InfixOp:
		writeLine("INFIX:")
		writeLine(n.Token.Lit)
		recPrint(b, indent+1, n.Left)
		recPrint(b, indent+1, n.Right)
	case Lambda:
		writeLine("LAMBDA:")
		recPrint(b, indent+1, n.Body)
	case FnCall:
		writeLine("FNCALL:")
		recPrint(b, indent+1, n.Fn)
		for _, e := range n.Args {
			recPrint(b, indent+1, e)
		}
	case Pipe:
		writeLine("PIPE:")
		recPrint(b, indent+1, n.Left)
		recPrint(b, indent+1, n.Right)
	case SnippetStmt:
		writeLine("SNIPPET:")
		writeLine(n.String())
	case ExprStmt:
		writeLine("EXPR:")
		recPrint(b, indent+1, n.X)
	case AssignStmt:
		writeLine("ASSIGN:")
		recPrint(b, indent+1, n.Name)
		recPrint(b, indent+1, n.X)
	default:
		//panic(fmt.Sprintf("unknown node type %T", n))
	}
}
