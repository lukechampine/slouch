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

type Expression interface {
	Node
}

type Identifier struct {
	Token token.Token // the IDENT token
	Name  string
}

func (i Identifier) isNode()        {}
func (i Identifier) String() string { return i.Name }

// e.g. ( 1 2 3 )
type Quotation struct {
	Token token.Token
	Body  []Expression
}

func (q Quotation) isNode() {}
func (q Quotation) String() string {
	strs := make([]string, len(q.Body))
	for i, t := range q.Body {
		strs[i] = t.String()
	}
	return "( " + strings.Join(strs, " ") + " )"
}

// e.g. 123
type Integer struct {
	Token token.Token
	Value string
}

func (i Integer) isNode()        {}
func (i Integer) String() string { return i.Value }

// e.g. "123"
type String struct {
	Token token.Token
	Value string
}

func (s String) isNode()        {}
func (s String) String() string { return strconv.Quote(s.Value) }

type Program struct {
	Expressions []Expression
}

func (p Program) isNode() {}

func (p Program) String() string {
	strs := make([]string, len(p.Expressions))
	for i := range strs {
		strs[i] = p.Expressions[i].String()
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
		for _, e := range n.Expressions {
			recPrint(b, indent+1, e)
		}
	case Integer:
		writeLine("INTEGER: " + n.String())
	case String:
		writeLine("STRING: " + n.String())
	case Identifier:
		writeLine("IDENTIFIER: " + n.String())
	case Quotation:
		writeLine("QUOTATION:")
		for _, e := range n.Body {
			recPrint(b, indent+1, e)
		}
	default:
		panic("unknown node type")
	}
}
