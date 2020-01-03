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

type Builtin struct {
	Token token.Token // the IDENT token
	Name  string
}

func (b Builtin) isNode()        {}
func (b Builtin) String() string { return b.Name }

type Identifier struct {
	Token token.Token // the IDENT token
	Name  string
}

func (i Identifier) isNode()        {}
func (i Identifier) String() string { return i.Name }

// e.g. foo 1 2 3
type FunctionCall struct {
	Token token.Token // the fn token
	Fn    Expression
	Args  []Expression
}

func (fc FunctionCall) isNode() {}
func (fc FunctionCall) String() string {
	strs := make([]string, 0, 1+len(fc.Args))
	strs = append(strs, fc.Fn.String())
	for _, a := range fc.Args {
		strs = append(strs, a.String())
	}
	return strings.Join(strs, " ")
}

// e.g. | sum
type Lambda struct {
	Token token.Token // the pipe token
	Body  Expression
}

func (l Lambda) isNode() {}
func (l Lambda) String() string {
	return "| " + l.Body.String()
}

// e.g. -f(x)
type PrefixOp struct {
	Token    token.Token // The prefix token, e.g. !
	Operator token.Kind
	Right    Expression
}

func (pe PrefixOp) isNode() {}
func (pe PrefixOp) String() string {
	return string(pe.Operator) + pe.Right.String()
}

// e.g. foo * bar
type InfixOp struct {
	Token    token.Token // The operator token, e.g. |
	Left     Expression
	Operator token.Kind
	Right    Expression
}

func (ie InfixOp) isNode() {}
func (ie InfixOp) String() string {
	return fmt.Sprintf("%v %v %v", ie.Left, ie.Operator, ie.Right)
}

// e.g. foo | bar
type Pipe struct {
	Token token.Token // The | token
	Left  Expression
	Right Expression
}

func (p Pipe) isNode() {}
func (p Pipe) String() string {
	return fmt.Sprintf("%v | %v", p.Left, p.Right)
}

// e.g. foo |= $bar
type Assignment struct {
	Token token.Token // The |= token
	Var   Identifier
	Value Expression
}

func (a Assignment) isNode() {}
func (a Assignment) String() string {
	return fmt.Sprintf("%v | %v", a.Value, a.Var)
}

// e.g. [a b c]
type Array struct {
	Elements []Expression
}

func (ae Array) isNode() {}
func (ae Array) String() string {
	strs := make([]string, len(ae.Elements))
	for i := range strs {
		strs[i] = ae.Elements[i].String()
	}
	return "[" + strings.Join(strs, " ") + "]"
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

	case Array:
		writeLine("ARRAY:")
		for _, e := range n.Elements {
			recPrint(b, indent+1, e)
		}

	case Integer:
		writeLine("INTEGER: " + n.String())

	case String:
		writeLine("STRING: " + n.String())

	case Identifier:
		writeLine("IDENTIFIER:")
		writeLine(n.String())

	case Builtin:
		writeLine("BUILTIN:")
		writeLine(n.String())

	case FunctionCall:
		writeLine("CALL:")
		recPrint(b, indent+1, n.Fn)
		for _, e := range n.Args {
			recPrint(b, indent+1, e)
		}

	case Lambda:
		writeLine("LAMBDA:")
		recPrint(b, indent+1, n.Body)

	case PrefixOp:
		writeLine("PREFIX:")
		writeLine("  " + n.Operator.String())
		recPrint(b, indent+1, n.Right)

	case InfixOp:
		writeLine("INFIX:")
		recPrint(b, indent+1, n.Left)
		writeLine("  " + n.Operator.String())
		recPrint(b, indent+1, n.Right)

	case Pipe:
		writeLine("PIPE:")
		recPrint(b, indent+1, n.Left)
		recPrint(b, indent+1, n.Right)

	case Assignment:
		writeLine("ASSIGNMENT:")
		recPrint(b, indent+1, n.Value)
		recPrint(b, indent+1, n.Var)

	default:
		panic("unknown node type")
	}
}
