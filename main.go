package main

import (
	"bytes"
	"fmt"
	"io"
	"log"
	"net/http"
	"os"
	"strconv"
	"strings"
	"sync"
	"time"

	"github.com/chzyer/readline"
	"lukechampine.com/slouch/ast"
	"lukechampine.com/slouch/bytecode"
	"lukechampine.com/slouch/lexer"
	"lukechampine.com/slouch/parser"
)

func main() {
	log.SetFlags(0)
	if len(os.Args) == 2 && os.Args[1] == "repl" {
		runRepl()
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

	c := bytecode.NewCompiler()
	program, err := c.Compile(p)
	if err != nil {
		log.Fatalln("couldn't compile program:", err)
	}
	err = bytecode.NewVM(func(v bytecode.Value) {
		fmt.Println(renderValue(v, true))
	}).Run(program, bytecode.ValString(input))
	if err != nil {
		panic(err)
	}
	fmt.Println("\nFinished in", time.Since(start))
}

func runRepl() {
	var input, solution string
	var day, year, part int
	var unquote bool
	prompt, err := readline.New("slouch> ")
	if err != nil {
		log.Fatal(err)
	}
	defer prompt.Close()
	prev := &evalPreview{
		prog:  &ast.Program{},
		input: input,
		term:  prompt.Terminal,
	}
	prompt.Config.Listener = prev
	logf := func(s string, args ...any) {
		fmt.Fprintf(prompt, "\r"+s, args...)
	}
	log := func(args ...any) {
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
			solution, err = executeOne(prev.prog, input, line, unquote)
			prev.mu.Unlock()
			if err != nil {
				log("Error:", err)
			} else {
				log(solution)
				// remember assignments
				for _, stmt := range parser.Parse(lexer.Tokenize(line)).Stmts {
					if as, ok := stmt.(ast.AssignStmt); ok {
						prev.prog.Stmts = append(prev.prog.Stmts, as)
					}
				}
			}
			continue
		}
		args := strings.Fields(line)
		switch args[0] {
		case ":reset":
			prev.prog = &ast.Program{}
			log("Reset evaluator")

		case ":setinput":
			input, err = readFile(args[1])
			if err != nil {
				log("Couldn't load input:", err)
				break
			}
			prev.input = input
			prev.prog = &ast.Program{}

		case ":load":
			data, err := os.ReadFile(args[2])
			if err != nil {
				log("Couldn't load file:", err)
				break
			}
			prev.prog.Stmts = append(prev.prog.Stmts, ast.AssignStmt{
				Name: ast.Ident{Name: args[1]},
				X:    ast.String{Value: string(data)},
			})

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
			prev.prog = &ast.Program{}
			logf("Ready for Dec %v, %v!\n", day, year)

		case ":run":
			prog, err := os.ReadFile(args[1])
			if err != nil {
				log("Couldn't load program:", err)
				break
			}
			if err := execute(prev.prog, input, string(prog), prompt.Terminal, unquote); err != nil {
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

		case ":disasm":
			p := parser.Parse(lexer.Tokenize(strings.TrimPrefix(line, ":disasm ")))
			p.Stmts = append(prev.prog.Stmts[:len(prev.prog.Stmts):len(prev.prog.Stmts)], p.Stmts...)
			log(p.String())
			program, err := bytecode.NewCompiler().Compile(p)
			if err != nil {
				log("Error:", err)
			} else {
				log(program.String())
			}

		default:
			log("Unrecognized command")
		}
	}
}

func renderValue(v bytecode.Value, unquote bool) string {
	switch v := v.(type) {
	case bytecode.ValString:
		if unquote {
			s, _ := strconv.Unquote(v.String())
			return s
		} else {
			return v.String()
		}
	case *bytecode.ValIcicle:
		return v.Collect().String()
	default:
		return v.String()
	}
}

func execute(pp *ast.Program, input, prog string, w io.Writer, unquote bool) (err error) {
	defer func() {
		if r := recover(); r != nil {
			err = fmt.Errorf("%v", r)
		}
	}()
	p := parser.Parse(lexer.Tokenize(prog))
	p.Stmts = append(pp.Stmts[:len(pp.Stmts):len(pp.Stmts)], p.Stmts...)
	var program bytecode.Program
	program, err = bytecode.NewCompiler().Compile(p)
	if err != nil {
		return
	}
	err = bytecode.NewVM(func(v bytecode.Value) {
		fmt.Fprint(w, "\r"+renderValue(v, unquote))
	}).Run(program, bytecode.ValString(input))
	return
}

func executeOne(pp *ast.Program, input, prog string, unquote bool) (output string, err error) {
	var buf bytes.Buffer
	err = execute(pp, input, prog, &buf, unquote)
	output = strings.TrimSpace(buf.String())
	return
}

type evalPreview struct {
	prog       *ast.Program
	term       *readline.Terminal
	input      string
	lastProg   string
	lastOutput string
	disasm     bool
	mu         sync.Mutex
}

func (p *evalPreview) OnChange(line []rune, pos int, key rune) (newLine []rune, newPos int, ok bool) {
	p.mu.Lock()
	defer p.mu.Unlock()
	if strings.HasPrefix(string(line), ":") {
		return
	}

	switch key {
	case readline.CharNext:
		p.disasm = !p.disasm
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
	}

	prog := strings.TrimRight(string(line), "| ")
	if prog != p.lastProg || key == readline.CharNext {
		if p.disasm {
			dis := parser.Parse(lexer.Tokenize(prog))
			dis.Stmts = append(p.prog.Stmts[:len(p.prog.Stmts):len(p.prog.Stmts)], dis.Stmts...)
			program, err := bytecode.NewCompiler().Compile(dis)
			if err != nil {
				p.lastOutput = fmt.Sprintf("Error: %v", err)
			} else {
				p.lastOutput = program.String()
			}
			p.lastProg = prog
		} else {
			output, err := executeOne(p.prog, p.input, prog, false)
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
	}

	rows := strings.Count(p.lastOutput, "\n") + 1
	fmt.Fprintf(p.term, "\n%v\033[%dF\033[%dC", p.lastOutput, rows, len("slouch> ")+pos)
	return
}

func getToken() string {
	token := os.Getenv("AOC_TOKEN")
	if token == "" {
		data, err := os.ReadFile("token.txt")
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
	data, err := io.ReadAll(resp.Body)
	if err != nil {
		log.Fatal(err)
	}
	return data
}

func getInput(year, day int) []byte {
	req, _ := http.NewRequest(http.MethodGet, fmt.Sprintf("https://adventofcode.com/%v/day/%v/input", year, day), nil)
	return doReq(req)
}

func postSolution(year, day, part int, answer string) string {
	body := strings.NewReader(fmt.Sprintf("level=%v&answer=%v", part, answer))
	req, _ := http.NewRequest(http.MethodPost, fmt.Sprintf("https://adventofcode.com/%v/day/%v/answer", year, day), body)
	req.Header.Set("Content-Type", "application/x-www-form-urlencoded")

	resp := string(doReq(req))
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
	bytes, err := os.ReadFile(filename)
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
		if err := os.WriteFile(filename, getInput(year, day), 0660); err != nil {
			log.Fatal(err)
		}
	}

	return readFile(filename)
}
