// Package ast ...
package ast

import (
	"fmt"
	"sort"
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

// e.g. "_"
type Hole struct {
	Token token.Token
}

func (h Hole) isNode()        {}
func (h Hole) isExpr()        {}
func (h Hole) String() string { return "_" }

// e.g. "-< foo"
type Splat struct {
	Token token.Token
	Fn    Expr
}

func (s Splat) isNode()        {}
func (s Splat) isExpr()        {}
func (s Splat) String() string { return "-< " + s.Fn.String() }

// e.g. "-: foo"
type Rep struct {
	Token token.Token
	Fn    Expr
}

func (r Rep) isNode()        {}
func (r Rep) isExpr()        {}
func (r Rep) String() string { return "-: " + r.Fn.String() }

// e.g. [ 1 2 3 ]
type Array struct {
	Token token.Token
	Elems []Expr
	Assoc bool
}

func (a Array) isNode() {}
func (a Array) isExpr() {}
func (a Array) String() string {
	strs := make([]string, len(a.Elems))
	if a.Assoc {
		for i := 0; i < len(a.Elems); i += 2 {
			strs = append(strs, fmt.Sprintf("%v:%v", a.Elems[i], a.Elems[i+1]))
		}
	} else {
		for i, t := range a.Elems {
			strs[i] = t.String()
		}
	}
	return "[ " + strings.Join(strs, " ") + " ]"
}

// e.g. 2+2
type InfixOp struct {
	Token       token.Token
	Op          token.Kind
	Left, Right Expr
}

func (i InfixOp) isNode() {}
func (i InfixOp) isExpr() {}
func (i InfixOp) String() string {
	switch {
	case i.Left == nil && i.Right == nil:
		return fmt.Sprintf("(%v)", i.Token)
	case i.Left == nil:
		return fmt.Sprintf("(%v %v)", i.Token, i.Right)
	case i.Right == nil:
		return fmt.Sprintf("(%v %v)", i.Left, i.Token)
	default:
		return fmt.Sprintf("%v %v %v", i.Left, i.Token, i.Right)
	}
}

// e.g. { x * 2 }
type Lambda struct {
	Token token.Token
	Body  Expr
}

func (l Lambda) isNode()        {}
func (l Lambda) isExpr()        {}
func (l Lambda) String() string { return "{ " + l.Body.String() + " }" }
func (l Lambda) NumArgs() int {
	params := l.Params()
	argOrder := "xyzabcdefghijklmnopqrstuvw"
	return strings.Index(argOrder, params[len(params)-1]) - strings.Index(argOrder, params[0]) + 1
}
func (l Lambda) Params() []string {
	// recursively search for single-char identifiers in body
	seen := make(map[string]bool)
	var args []string
	Visit(l.Body, func(n Node) bool {
		switch n := n.(type) {
		case Lambda:
			// do not descend into other lambdas
			return false
		case Ident:
			if len(n.Name) == 1 && !seen[n.Name] {
				args = append(args, n.Name)
				seen[n.Name] = true
			}
			return false
		default:
			return true
		}
	})
	argOrder := "xyzabcdefghijklmnopqrstuvw"
	sort.Slice(args, func(i, j int) bool {
		return strings.Index(argOrder, args[i]) < strings.Index(argOrder, args[j])
	})
	return args
}

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

func Visit(n Node, fn func(Node) bool) {
	if !fn(n) {
		return
	}
	switch n := n.(type) {
	case Splat:
		Visit(n.Fn, fn)
	case Rep:
		Visit(n.Fn, fn)
	case Array:
		for _, e := range n.Elems {
			Visit(e, fn)
		}
	case InfixOp:
		Visit(n.Left, fn)
		Visit(n.Right, fn)
	case Lambda:
		Visit(n.Body, fn)
	case FnCall:
		Visit(n.Fn, fn)
		for _, e := range n.Args {
			Visit(e, fn)
		}
	case Pipe:
		Visit(n.Left, fn)
		Visit(n.Right, fn)
	case ExprStmt:
		Visit(n.X, fn)
	case AssignStmt:
		Visit(n.Name, fn)
		Visit(n.X, fn)
	case Program:
		for _, e := range n.Stmts {
			Visit(e, fn)
		}
	}
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
	case nil:
		writeLine("<nil>")
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
	case Hole:
		writeLine("HOLE")
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
	case Splat:
		writeLine("SPLAT:")
		recPrint(b, indent+1, n.Fn)
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
		panic(fmt.Sprintf("unknown node type %T", n))
	}
}
