// Package token ...
package token

import "strconv"

type Kind uint8

const (
	Illegal Kind = iota
	Ident
	Builtin
	Int
	String
	Pipe
	PipeEquals
	Lbrace
	Rbrace
	Lparen
	Rparen
	Bang
	Newline
	Plus
	Minus
)

func (k Kind) String() string {
	return [...]string{
		Illegal:    "Illegal",
		Ident:      "Ident",
		Builtin:    "Builtin",
		Int:        "Int",
		String:     "String",
		Pipe:       "Pipe",
		PipeEquals: "PipeEquals",
		Lbrace:     "Lbrace",
		Rbrace:     "Rbrace",
		Lparen:     "Lparen",
		Rparen:     "Rparen",
		Bang:       "Bang",
		Newline:    "Newline",
		Plus:       "Plus",
		Minus:      "Minus",
	}[k]
}

type Token struct {
	Kind Kind
	Lit  string
}

func (t Token) String() string { return t.Kind.String() + " " + strconv.Quote(t.Lit) }

func IsBuiltin(word string) bool {
	switch word {
	case
		"cat",
		"count",
		"inc",
		"sum":
		return true
	}
	return false
}
