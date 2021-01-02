// Package token ...
package token

import "strconv"

type Kind uint8

const (
	Illegal Kind = iota
	EOF
	Newline
	Ident
	Int
	String
	Assign
	Equals
	Comma
	Plus
	Star
	Snippet
	Lbrace
	Rbrace
	Lparen
	Rparen
	Pipe
	Buck
)

func (k Kind) String() string {
	return [...]string{
		Illegal: "Illegal",
		EOF:     "EOF",
		Newline: "Newline",
		Ident:   "Ident",
		Int:     "Int",
		String:  "String",
		Assign:  "Assign",
		Equals:  "Equals",
		Comma:   "Comma",
		Plus:    "Plus",
		Star:    "Star",
		Snippet: "Snippet",
		Lbrace:  "Lbrace",
		Rbrace:  "Rbrace",
		Lparen:  "Lparen",
		Rparen:  "Rparen",
		Pipe:    "Pipe",
		Buck:    "Buck",
	}[k]
}

type Token struct {
	Kind Kind
	Lit  string
}

func (t Token) String() string { return t.Kind.String() + " " + strconv.Quote(t.Lit) }
