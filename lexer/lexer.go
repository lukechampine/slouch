// Package lexer ...
package lexer

import (
	"strings"

	"lukechampine.com/slouch/token"
)

var singleCharTokens = map[byte]token.Kind{
	'=':  token.Assign,
	'+':  token.Plus,
	'-':  token.Neg,
	'*':  token.Star,
	'/':  token.Slash,
	'%':  token.Mod,
	',':  token.Comma,
	'.':  token.Dot,
	':':  token.Colon,
	'(':  token.Lparen,
	')':  token.Rparen,
	'[':  token.Lbrace,
	']':  token.Rbrace,
	'{':  token.Lbracket,
	'}':  token.Rbracket,
	'<':  token.Less,
	'>':  token.Greater,
	'|':  token.Pipe,
	'$':  token.Buck,
	'_':  token.Hole,
	'\n': token.Newline,
}

func isSingleCharToken(c byte) bool {
	_, ok := singleCharTokens[c]
	return ok
}

var doubleCharTokens = map[string]token.Kind{
	"==": token.Equals,
	"!=": token.NotEquals,
	"<=": token.LessEquals,
	">=": token.GreaterEquals,
	"%?": token.DivisibleBy,
	"-<": token.Splat,
	"|<": token.PipeSplat,
	"-:": token.Rep,
	"|:": token.PipeRep,
}

func isDoubleCharToken(s string) bool {
	_, ok := doubleCharTokens[s]
	return ok
}

func isDigit(c byte) bool  { return '0' <= c && c <= '9' }
func isLetter(c byte) bool { return ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') }
func isIdent(c byte) bool  { return isDigit(c) || isLetter(c) || c == '_' }

// Tokenize lexes s as a sequence of tokens.
func Tokenize(s string) (ts []token.Token) {
	for i := 0; i < len(s); i++ {
		switch c := s[i]; {
		case c == ' ', c == '\t':
			// skip

		case c == ';':
			// comment; skip until \n
			i++
			for i < len(s) && s[i] != '\n' {
				i++
			}

		case i+1 < len(s) && isDoubleCharToken(s[i:i+2]):
			lit := string(s[i : i+2])
			ts = append(ts, token.Token{
				Kind: doubleCharTokens[lit],
				Lit:  lit,
			})
			i++

		case c == '-' && i+1 < len(s) && isDigit(s[i+1]):
			// negative number
			num := string(s[i])
			for i++; i < len(s) && isDigit(s[i]); i++ {
				num += string(s[i])
			}
			i--
			ts = append(ts, token.Token{
				Kind: token.Int,
				Lit:  num,
			})

		case isSingleCharToken(c):
			ts = append(ts, token.Token{
				Kind: singleCharTokens[c],
				Lit:  string(c),
			})

		// string
		case c == '"':
			str := ""
			i++
			for i < len(s) && !(s[i] == '"' && s[i-1] != '\\') {
				str += string(s[i])
				i++
			}
			ts = append(ts, token.Token{
				Kind: token.String,
				Lit:  `"` + str + `"`,
			})

		// number
		case isDigit(c):
			num := ""
			for i < len(s) && isDigit(s[i]) {
				num += string(s[i])
				i++
			}
			i--
			ts = append(ts, token.Token{
				Kind: token.Int,
				Lit:  num,
			})

		// identifier (or special infix, or snippet)
		case isLetter(c):
			word := ""
			for i < len(s) && isIdent(s[i]) {
				word += string(s[i])
				i++
			}
			switch word {
			case "and":
				ts = append(ts, token.Token{
					Kind: token.And,
					Lit:  word,
				})
			case "or":
				ts = append(ts, token.Token{
					Kind: token.Or,
					Lit:  word,
				})
			case "vvv":
				// scan until ^^^
				if stop := strings.Index(s[i:], "^^^"); stop == -1 {
					ts = append(ts, token.Token{
						Kind: token.Illegal,
						Lit:  word + s[i:],
					})
					i = len(s)
				} else {
					ts = append(ts, token.Token{
						Kind: token.Snippet,
						Lit:  s[i:][:stop],
					})
					i += stop + 3
				}
			default:
				ts = append(ts, token.Token{
					Kind: token.Ident,
					Lit:  word,
				})
			}
			i--

		default:
			ts = append(ts, token.Token{
				Kind: token.Illegal,
				Lit:  string(c),
			})
		}
	}
	return
}
