// Package lexer ...
package lexer

import "lukechampine.com/slouch/token"

import "strings"

func isDigit(c byte) bool     { return '0' <= c && c <= '9' }
func isLetter(c byte) bool    { return ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') }
func isIdent(c byte) bool     { return isDigit(c) || isLetter(c) || c == '_' }
func isBuiltinOp(c byte) bool { return strings.IndexByte("[]+-*/", c) >= 0 }

func Tokenize(s string) (ts []token.Token) {
	for i := 0; i < len(s); i++ {
		switch c := s[i]; {
		case c == ' ', c == '\t', c == '\n':
			// skip

		case c == '(':
			ts = append(ts, token.Token{
				Kind: token.Lparen,
				Lit:  string(c),
			})

		case c == ')':
			ts = append(ts, token.Token{
				Kind: token.Rparen,
				Lit:  string(c),
			})

		case isBuiltinOp(c):
			if c == '-' && i+1 < len(s) && isDigit(s[i+1]) {
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
			} else {
				ts = append(ts, token.Token{
					Kind: token.Ident,
					Lit:  string(c),
				})
			}

		case c == '"':
			str := ""
			i++
			for i < len(s) && s[i] != '"' {
				str += string(s[i])
				i++
			}
			ts = append(ts, token.Token{
				Kind: token.String,
				Lit:  str,
			})

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

		case isLetter(c):
			word := ""
			for i < len(s) && isIdent(s[i]) {
				word += string(s[i])
				i++
			}
			i--
			ts = append(ts, token.Token{
				Kind: token.Ident,
				Lit:  word,
			})

		default:
			ts = append(ts, token.Token{
				Kind: token.Illegal,
				Lit:  string(c),
			})
		}
	}
	return
}
