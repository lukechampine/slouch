// Package lexer ...
package lexer

import "lukechampine.com/slouch/token"

var byteTokens = map[byte]token.Kind{
	'[':  token.Lbrace,
	']':  token.Rbrace,
	'(':  token.Lparen,
	')':  token.Rparen,
	'\n': token.Newline,
	'!':  token.Bang,
	'+':  token.Plus,
	'-':  token.Minus,
}

func isByteToken(c byte) bool { return byteTokens[c] != token.Illegal }
func isDigit(c byte) bool     { return '0' <= c && c <= '9' }
func isLetter(c byte) bool    { return ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') }
func isIdent(c byte) bool     { return isDigit(c) || isLetter(c) || c == '_' }

func Tokenize(s string) (ts []token.Token) {
	for i := 0; i < len(s); i++ {
		switch c := s[i]; {
		case c == ' ', c == '\t':
			// skip

		case isByteToken(c):
			ts = append(ts, token.Token{
				Kind: byteTokens[c],
				Lit:  string(c),
			})

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

		case c == '$':
			id := ""
			i++
			for i < len(s) && isIdent(s[i]) {
				id += string(s[i])
				i++
			}
			i--
			ts = append(ts, token.Token{
				Kind: token.Ident,
				Lit:  id,
			})

		case c == '|':
			t := token.Token{
				Kind: token.Pipe,
				Lit:  string(c),
			}
			if i+1 < len(s) {
				switch s[i+1] {
				case '=':
					t.Kind = token.PipeEquals
					t.Lit += string(s[i+1])
					i++
				default:
					// normal pipe
				}
			}
			ts = append(ts, t)

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
			if token.IsBuiltin(word) {
				ts = append(ts, token.Token{
					Kind: token.Builtin,
					Lit:  word,
				})
			} else {
				ts = append(ts, token.Token{
					Kind: token.Illegal,
					Lit:  word,
				})
			}

		default:
			ts = append(ts, token.Token{
				Kind: token.Illegal,
				Lit:  string(c),
			})
		}
	}
	return
}
