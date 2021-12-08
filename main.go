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
	"lukechampine.com/slouch/evaluator"
	"lukechampine.com/slouch/lexer"
	"lukechampine.com/slouch/parser"
)

/*

	BUGS:
	- [ ] "iterator aliasing"
			=iot take _ iota
			[(iot 3), (iot 4), (iot 2)]
		* should return [[0, 1, 2], [0, 1, 2, 3], [0, 1]], instead returns [[0, 1, 2], [3, 4, 5, 6], [7, 8]]
			=iot take _ iota
			[(iot 3), (iot 4), (iot 2)] | first (len == 4)
		* should return [0, 1, 2, 3], instead returns []
		* in the general case, this requires analyzing the full program ahead of time...
		* for now, just make (deep!) copies everywhere
		* also need to duplicate iterator in partial fn args
		*
		* "garbage collection" -- use ref counts, and .clone when multiple refs exist
			* probably need to add .clone field to iterator

	PERF:
	- [ ] special case []int64 and []string for arrays (or iterators?)
	- [ ] track iterator size, for O(1) len and more efficient collect
		* not possible for e.g. iota | takeWhile (<10)
	- [ ] panic(fmt.Sprintf("%T", v)) causes v to escape
	- [ ] refcount all values; reuse memory if 0
	- [ ] memoization rune or builtin
		* e.g. memo (last x) only computes (last x) once

	TODO:
	- [x] proper test framework
	- [x] start tracking how many AoC problems have (complete, working, idiomatic, performant) solutions
	- [ ] cleanup Builtin/Partial distinction; should just have one FuncValue type
	- [ ] reconsider automatically converting strings to iterator
		* chars builtin is probably less surprising
	- [ ] make _ work in arrays
	- [ ] vscode/lsp
		* make a language server that handles highlighting, autocomplete, etc.
			* but also, onchange, re-run and send results to a "webview" extension

	IDEAS:
	- [ ] make "truthy" explicit, never implicit
	- [ ] assign to global within expr
		e.g. first -<(d=(diff) | len == 1) | delete d
	- [/] paste problem description into AI; spits out possibly-relevant functions
		* probably better to have human-directed keyword search (e.g. "circular" spits out cycle)
	- [ ] time.Parse-style API for directions: parsedirs "U1 D2 L3 R4", parsedirs "N:1 S:2 W:3 E:4"

2017 day 14
2018 day 15
2018 day 23
2019 day 18
2020 day 20

*/

// terminal niceties:
// - [x] readline (particularly history)
// - [x] display result as you type, one line below
//       * truncate as necessary
// - [ ] colors
// - [ ] tab completion
// - [ ] timeout
// - [ ] store result of each repl entry in r1, r2, etc.
// - [ ] key bindings:
//	   - [ ] ctrl-s: save current result to variable
//	   - [ ] ctrl-l: drop into sub-evaluator where input is the first element of the current result; esc appends current prog to old cursor

func main() {
	if len(os.Args) > 1 && os.Args[1] == "repl" {
		log.SetFlags(0)

		var input, solution string
		var day, year, part int
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
				solution, err = executeOne(eval, input, line)
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
				if err := execute(eval, input, string(prog), prompt.Terminal); err != nil {
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
	err = evaluator.New().Run(p, input, func(v evaluator.Value) { fmt.Println(v) })
	if err != nil {
		panic(err)
	}
	fmt.Println("\nFinished in", time.Since(start))
}

func execute(eval *evaluator.Environment, input, prog string, w io.Writer) (err error) {
	func() {
		defer func() {
			if r := recover(); r != nil {
				err = fmt.Errorf("%v", r)
			}
		}()
		p := parser.Parse(lexer.Tokenize(prog))
		err = eval.Run(p, input, func(v evaluator.Value) {
			fmt.Fprintln(w, "\r"+v.String())
		})
	}()
	return
}

func executeOne(eval *evaluator.Environment, input, prog string) (output string, err error) {
	var buf bytes.Buffer
	err = execute(eval, input, prog, &buf)
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
		output, err := executeOne(p.eval.Clone(), p.input, prog)
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

func createCookieFile(cookie string) error {
	return ioutil.WriteFile("cookie.txt", []byte(cookie), 0660)
}

func getCookieFromFile() string {
	filename := "cookie.txt"
	if _, err := os.Stat(filename); err != nil {
		if err != nil {
			log.Println("Cookie file not found! Please place your token into AOC_SESSION or cookie.txt")
		}
		if err := createCookieFile(""); err != nil {
			log.Fatal(err)
		}
	}
	input, _ := ioutil.ReadFile(filename)
	return strings.TrimSpace(string(input))
}

func getCookie() string {
	cookie := os.Getenv("AOC_TOKEN")
	if len(cookie) == 0 {
		log.Println("Unable to fetch cookie automatically, reading from file...")
		cookie = getCookieFromFile()
	}
	return cookie
}

func doReq(req *http.Request) []byte {
	req.AddCookie(&http.Cookie{
		Name:  "session",
		Value: getCookie(),
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
	return strings.TrimSpace(string(bytes)), err
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
