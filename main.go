package main

import (
	"bytes"
	"fmt"
	"io"
	"io/ioutil"
	"log"
	"net/http"
	"os"
	"strconv"
	"strings"
	"sync"
	"time"

	"github.com/chzyer/readline"
	"github.com/pkg/profile"
	"lukechampine.com/slouch/ast"
	"lukechampine.com/slouch/evaluator"
	"lukechampine.com/slouch/lexer"
	"lukechampine.com/slouch/parser"
)

func main() {
	if len(os.Args) > 1 && os.Args[1] == "repl" {
		log.SetFlags(0)

		var input, solution string
		var day, year, part int
		var unquote bool
		eval := evaluator.New()
		prompt, err := readline.New("slouch> ")
		if err != nil {
			log.Fatal(err)
		}
		defer prompt.Close()
		prev := &evalPreview{eval: eval, term: prompt.Terminal}
		prompt.Config.Listener = prev
		logf := func(s string, args ...interface{}) {
			fmt.Fprintf(prompt, "\r"+s, args...)
		}
		log := func(args ...interface{}) {
			fmt.Fprint(prompt, "\r"+fmt.Sprintln(args...))
		}
		for {
			line, err := prompt.Readline()
			if err == io.EOF {
				break
			} else if err != nil {
				log("Couldn't read input", err)
				return
			} else if len(line) == 0 {
				continue
			}

			if !strings.HasPrefix(line, ":") {
				prev.mu.Lock()
				solution, err = executeOne(eval, input, line, unquote)
				prev.mu.Unlock()
				if err != nil {
					log("Error:", err)
				} else {
					log(solution)
				}
				continue
			}
			args := strings.Fields(line)
			switch args[0] {
			case ":reset":
				eval = evaluator.New()
				prev.eval = eval
				log("Reset evaluator")

			case ":setinput":
				input, err = readFile(args[1])
				if err != nil {
					log("Couldn't load input:", err)
					break
				}
				prev.input = input
				eval = evaluator.New()
				prev.eval = eval

			case ":load":
				data, err := ioutil.ReadFile(args[2])
				if err != nil {
					log("Couldn't load file:", err)
					break
				}
				eval.Bind(args[1], eval.Eval(ast.String{Value: string(data)}))

			case ":setquote":
				unquote = false
				log("Unquote mode on: string values will be escaped before display")

			case ":setunquote":
				unquote = true
				log("Unquote mode on: string values will be displayed raw")

			case ":setday":
				day, _ = strconv.Atoi(args[1])
				year, _ = strconv.Atoi(args[2])
				part = 1
				input, err = fetchInput(day, year)
				if err != nil {
					log("Couldn't load input:", err)
					break
				}
				prev.input = input
				eval = evaluator.New()
				prev.eval = eval
				logf("Ready for Dec %v, %v!\n", day, year)

			case ":run":
				prog, err := ioutil.ReadFile(args[1])
				if err != nil {
					log("Couldn't load program:", err)
					break
				}
				if err := execute(eval, input, string(prog), prompt.Terminal, unquote); err != nil {
					log("Error:", err)
				}

			case ":submit":
				if solution == "" {
					solution = args[1]
				}
				logf("Submitting %v for part %v...\n", solution, part)
				log(postSolution(year, day, part, solution))
				part++
				solution = ""

			default:
				log("Unrecognized command")
			}
		}
		return
	}

	if len(os.Args) != 3 {
		log.Fatal("Usage: slouch <prog> <input>")
	}
	prog, err := readFile(os.Args[1])
	if err != nil {
		log.Fatalln("couldn't read program:", err)
	}
	input, err := readFile(os.Args[2])
	if err != nil {
		log.Fatalln("couldn't read input:", err)
	}
	p := parser.Parse(lexer.Tokenize(prog))
	start := time.Now()
	err = evaluator.New().Run(p, input, func(v evaluator.Value) {
		if s, ok := v.(*evaluator.StringValue); ok {
			raw, _ := strconv.Unquote(s.String()) // hack
			fmt.Println(raw)
		} else {
			fmt.Println(v)
		}
	})
	if err != nil {
		panic(err)
	}
	fmt.Println("\nFinished in", time.Since(start))
}

func execute(eval *evaluator.Environment, input, prog string, w io.Writer, unquote bool) (err error) {
	func() {
		defer func() {
			if r := recover(); r != nil {
				err = fmt.Errorf("%v", r)
			}
		}()
		p := parser.Parse(lexer.Tokenize(prog))
		err = eval.Run(p, input, func(v evaluator.Value) {
			if s, ok := v.(*evaluator.StringValue); ok && unquote {
				raw, _ := strconv.Unquote(s.String())
				fmt.Fprintln(w, "\r"+raw)
			} else {
				fmt.Fprintln(w, "\r"+v.String())
			}
		})
	}()
	return
}

func executeOne(eval *evaluator.Environment, input, prog string, unquote bool) (output string, err error) {
	var buf bytes.Buffer
	err = execute(eval, input, prog, &buf, unquote)
	output = strings.TrimSpace(buf.String())
	return
}

var closingChar = map[rune]rune{
	'(': ')', '{': '}', '[': ']',
	')': '(', '}': '{', ']': '[',
}

type evalPreview struct {
	eval       *evaluator.Environment
	term       *readline.Terminal
	input      string
	lastProg   string
	lastOutput string
	mu         sync.Mutex
}

func (p *evalPreview) OnChange(line []rune, pos int, key rune) (newLine []rune, newPos int, ok bool) {
	p.mu.Lock()
	defer p.mu.Unlock()
	if strings.HasPrefix(string(line), ":") {
		return
	}
	switch key {
	case '(', '{', '[':
		// paren/brace matching
		if pos == len(line) || line[pos] == ' ' {
			closer := map[rune]rune{'(': ')', '{': '}', '[': ']'}[key]
			newLine = append(append(line[:pos:pos], closer), line[pos:]...)
			return newLine, pos, true
		}
	case ')', '}', ']':
		// overwrite closing char
		if 0 < pos && pos < len(line) && line[pos-1] == key && line[pos] == key {
			return append(line[:pos-1], line[pos:]...), pos, true
		}
		// case readline.CharBackspace, readline.CharDelete:
		// 	// delete matching paren/brace
		// 	if pos < len(line) && pos < len(p.lastProg) {
		// 		switch c := p.lastProg[pos]; c {
		// 		case '(', '{', '[':
		// 			closer := map[byte]rune{'(': ')', '{': '}', '[': ']'}[c]
		// 			if line[pos] == closer {
		// 				line = append(line[:pos], line[pos+1:]...)
		// 				return line, pos, true
		// 			}
		// 		}
		// 	}
	}

	prog := strings.TrimRight(string(line), "| ")
	if prog != p.lastProg {
		output, err := executeOne(p.eval.Clone(), p.input, prog, false)
		if err != nil {
			if !strings.HasSuffix(string(line), " ") && strings.Contains(err.Error(), "identifier") {
				return
			}
			output = fmt.Sprintf("Error: %v", err)
		}
		w := p.term.GetConfig().FuncGetWidth()
		if len(output) > w {
			output = output[:w-20] + "..." + output[len(output)-15:]
		}
		p.lastProg = prog
		p.lastOutput = output
	}
	fmt.Fprintf(p.term, "\n%v", p.lastOutput)                   // write output on next line
	fmt.Fprintf(p.term, "\033[1F\033[%dC", len("slouch> ")+pos) // restore cursor
	return
}

func getToken() string {
	token := os.Getenv("AOC_TOKEN")
	if token == "" {
		data, err := ioutil.ReadFile("token.txt")
		if err != nil {
			log.Fatal("Token not found! Please set AOC_TOKEN or put it in token.txt.")
		}
		token = string(data)
	}
	return token
}

func doReq(req *http.Request) []byte {
	req.AddCookie(&http.Cookie{
		Name:  "session",
		Value: getToken(),
	})
	resp, err := http.DefaultClient.Do(req)
	if err != nil {
		log.Fatal(err)
	}
	defer resp.Body.Close()
	data, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		log.Fatal(err)
	}
	return data
}

func getPuzzle(year, day int) string {
	req, _ := http.NewRequest(http.MethodGet, fmt.Sprintf("https://adventofcode.com/%v/day/%v", year, day), nil)
	return string(doReq(req))
}

func getInput(year, day int) []byte {
	req, _ := http.NewRequest(http.MethodGet, fmt.Sprintf("https://adventofcode.com/%v/day/%v/input", year, day), nil)
	return doReq(req)
}

func postSolution(year, day, part int, answer string) string {
	body := strings.NewReader(fmt.Sprintf("level=%v&answer=%v", part, answer))
	req, _ := http.NewRequest(http.MethodPost, fmt.Sprintf("https://adventofcode.com/%v/day/%v/answer", year, day), body)
	req.Header.Set("Content-Type", "application/x-www-form-urlencoded")
	return extractMain(string(doReq(req)))
}

func extractMain(resp string) string {
	i := strings.Index(resp, "<main>")
	if i == -1 {
		return resp
	}
	resp = strings.TrimSpace(resp[i+len("<main>") : strings.Index(resp, "</main>")])
	resp = strings.TrimSuffix(strings.TrimPrefix(resp, "<article>"), "</article>")
	resp = strings.TrimSuffix(strings.TrimPrefix(resp, "<p>"), "</p>")
	return resp
}

func readFile(filename string) (string, error) {
	bytes, err := ioutil.ReadFile(filename)
	return strings.TrimRight(string(bytes), " \t\n"), err
}

func fetchInput(day, year int) (string, error) {
	filename := fmt.Sprintf("%v_day%v.txt", year, day)
	if _, err := os.Stat(filename); err != nil {
		est, err := time.LoadLocation("EST")
		if err != nil {
			log.Fatal(err)
		}
		if t := time.Date(year, time.December, day, 0, 0, 0, 0, est); time.Until(t) > 0 {
			log.Printf("\rPuzzle not unlocked yet! Sleeping for %v...", time.Until(t).Round(time.Second))
			time.Sleep(time.Until(t) + 3*time.Second) // don't want to fire too early
		}
		log.Println("Downloading input...")
		if err := ioutil.WriteFile(filename, getInput(year, day), 0660); err != nil {
			log.Fatal(err)
		}
	}

	return readFile(filename)
}
