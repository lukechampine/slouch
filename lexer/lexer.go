// Package lexer ...
package lexer

import (
	"strings"

	"lukechampine.com/slouch/token"
)

var singleCharTokens = map[byte]token.Kind{
	'=':  token.Assign,
	'+':  token.Plus,
	'*':  token.Star,
	',':  token.Comma,
	'(':  token.Lparen,
	')':  token.Rparen,
	'[':  token.Lbrace,
	']':  token.Rbrace,
	'|':  token.Pipe,
	'$':  token.Buck,
	'\n': token.Newline,
}

func isSingleCharToken(c byte) bool {
	_, ok := singleCharTokens[c]
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

		case isSingleCharToken(c):
			if c == '=' && i+1 < len(s) && s[i+1] == '=' {
				lit := string(s[i : i+2])
				i++
				ts = append(ts, token.Token{
					Kind: token.Equals,
					Lit:  lit,
				})
				continue
			}
			if c == '-' && i+1 < len(s) && isDigit(s[i+1]) {
				// negative number, not minus
				num := string(s[i])
				for i++; i < len(s) && isDigit(s[i]); i++ {
					num += string(s[i])
				}
				i--
				ts = append(ts, token.Token{
					Kind: token.Int,
					Lit:  num,
				})
				continue
			}
			ts = append(ts, token.Token{
				Kind: singleCharTokens[c],
				Lit:  string(c),
			})

		// string
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

		// identifier (or snippet)
		case isLetter(c):
			word := ""
			for i < len(s) && isIdent(s[i]) {
				word += string(s[i])
				i++
			}
			if word == "vvv" {
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
			} else {
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
