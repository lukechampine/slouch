// Package token ...
package token

import "strconv"

type Kind uint8

const (
	Illegal Kind = iota
	Ident
	Int
	String
	Lbrace
	Rbrace
	Lparen
	Rparen
)

func (k Kind) String() string {
	return [...]string{
		Illegal: "Illegal",
		Ident:   "Ident",
		Int:     "Int",
		String:  "String",
		Lbrace:  "Lbrace",
		Rbrace:  "Rbrace",
		Lparen:  "Lparen",
		Rparen:  "Rparen",
	}[k]
}

type Token struct {
	Kind Kind
	Lit  string
}

func (t Token) String() string { return t.Kind.String() + " " + strconv.Quote(t.Lit) }
