package bytecode

import (
	"fmt"
	"sort"
	"strings"

	"github.com/peter-evans/patience"
	"lukechampine.com/slouch/token"
)

func (c *Compiler) passFoldConstants(pp *Program) {
	// pre:         post:
	// CONST 2      CONST 5
	// CONST 3
	// ADD
	program := *pp
	defer func() { *pp = program }()

	for i := 0; i < len(program); i++ {
		constArgs := func(n int) bool {
			if i-n < 0 {
				return false
			}
			for j := 0; j < n; j++ {
				if _, ok := program[i-n+j].(instConstant); !ok {
					return false
				}
			}
			return true
		}
		replace := func(n int, fn func(args []Value) Value) {
			if !constArgs(n) {
				return
			}
			args := make([]Value, n)
			for j := range args {
				args[j] = program[i-n+j].(instConstant).Value
			}
			program[i] = instConstant{Value: fn(args)}
			program = append(program[:i-n], program[i:]...)
		}

		switch in := program[i].(type) {
		case instArray:
			replace(in.Len, func(args []Value) Value { return ValArray(args) })
		case instAssoc:
			replace(in.Len*2, func(args []Value) Value { return makeAssoc(args) })
		case instPrefixOp:
			replace(1, func(args []Value) Value { return evalPrefixOp(in.Kind, args[0]) })
		case instInfixOp:
			// TODO: special case for Dot
			replace(2, func(args []Value) Value { return evalInfixOp(in.Kind, args[0], args[1]) })
		case instDup:
			if constArgs(1) {
				program[i] = program[i-1]
			}
		case instJumpEq:
			if constArgs(1) {
				v := program[i-1].(instConstant).Value
				if evalInfixOp(token.Equals, v, in.Value).(ValBool) {
					program[i] = instJump{Target: in.Target}
					program = append(program[:i-1], program[i:]...)
				} else {
					program = append(program[:i-1], program[i+1:]...)
				}
			}
		case instCall:
			// if constArgs(1) {
			// 	v := program[i-1].(instConstant).Value.(ValFunc)
			// 	// partially apply as many args as possible
			// 	// n := in.Arity
			// 	// for j := 0; j < n && i-2-n+j >= 0; j++ {
			// 	// 	if ic, ok := program[i-2-n+j].(instConstant); ok {
			// 	// 		v.Applied = append(v.Applied, ic.Value)
			// 	// 		in.Arity--
			// 	// 	} else {
			// 	// 		break
			// 	// 	}
			// 	// }
			// 	// program[i-1] = instConstant{v}
			// 	// program[i] = in
			// 	// if in.Arity == 0 {

			// 	// }
			// 	if bf, ok := builtins[v.Name]; ok && in.Arity == 0 {
			// 		replace(1+bf.Arity, func(args []Value) Value { return bf.Fn(args[:len(args)-1]) })
			// 	}
			// }
		}
	}
}

func (c *Compiler) passStoreLoad(pp *Program) {
	// pre:           post:
	// fn lambda1:    fn lambda1:
	//   ASSIGN y       ADD
	//   ASSIGN x       RETURN
	//   LOAD x
	//   LOAD y
	//   ADD
	//   RETURN
	program := *pp
	defer func() { *pp = program }()

	// only consider assignments that are loaded once
	assigns := make(map[string]int)
	for i := 0; i < len(c.program); i++ {
		if il, ok := c.program[i].(instLoad); ok {
			assigns[il.Name]++
		}
	}
	for _, cf := range c.fns {
		for i := 0; i < len(cf.Body); i++ {
			if il, ok := cf.Body[i].(instLoad); ok {
				assigns[il.Name]++
			}
		}
	}

	stackImpact := func(i Instruction) int {
		switch i := i.(type) {
		case instArray:
			return 1 - i.Len
		case instAssoc:
			return 1 - i.Len
		case instCall:
			return -(1 + i.Arity)
		case instInfixOp, instAssign, instPrefixOp, instOutput, instPop, instJump, instJumpEq:
			return -1
		case instTarget, instFuncDef, instReturn:
			return 0
		case instConstant, instLoad, instDup:
			return 1
		default:
			panic(fmt.Sprintf("unhandled instruction %T", i))
		}
	}

outer:
	for i := 0; i < len(program); i++ {
		if ia, ok := program[i].(instAssign); ok && i+1 < len(program) {
			if assigns[ia.Name] != 1 {
				continue
			}
			// scan forward, tracking stack height
			var height int
			for j := i + 1; j < len(program); j++ {
				if il, ok := program[j].(instLoad); ok && il.Name == ia.Name {
					if height != 0 {
						continue outer
					}
					// delete both the load and the store
					program = append(program[:j], program[j+1:]...)
					program = append(program[:i], program[i+1:]...)
					break
				} else {
					height += stackImpact(program[j])
				}
			}
		}
	}
}

func (c *Compiler) passDedup(pp *Program) {
	// pre:         post:
	// LOAD x       LOAD x
	// LOAD x       DUP
	program := *pp
	defer func() { *pp = program }()

	for i := 0; i+1 < len(program); i++ {
		if program[i].String() == program[i+1].String() {
			switch program[i].(type) {
			case instLoad:
				program[i+1] = instDup{}
			}
		}
	}
}

func (c *Compiler) passDeadCode(pp *Program) {
	// pre:           post:
	// JUMP label1    JUMP label1
	// CONST "yo"     :label1
	// OUTPUT
	// :label1
	program := *pp
	defer func() { *pp = program }()

outer:
	for i := 0; i < len(program); i++ {
		if _, ok := program[i].(instConstant); ok && i+1 < len(program) {
			if _, ok := program[i+1].(instPop); ok {
				program = append(program[:i], program[i+2:]...)
			}
		} else if ij, ok := program[i].(instJump); ok {
			for j := i + 1; j < len(program); j++ {
				if il, ok := program[j].(instTarget); ok {
					if ij.Target == il.Name {
						program = append(program[:i], program[j:]...)
					} else {
						program = append(program[:i+1], program[j:]...)
					}
					continue outer
				}
			}
		}
	}
}

func (c *Compiler) passUnusedLabels(pp *Program) {
	// pre:           post:
	// CONST true     CONST true
	// :label1 	      OUTPUT
	// OUTPUT
	program := *pp
	defer func() { *pp = program }()

	isTarget := make(map[string]bool)
	for i := 0; i < len(program); i++ {
		if ij, ok := program[i].(instJump); ok {
			isTarget[ij.Target] = true
		} else if ij, ok := program[i].(instJumpEq); ok {
			isTarget[ij.Target] = true
		}
	}
	for i := 0; i < len(program); i++ {
		if il, ok := program[i].(instTarget); ok {
			if !isTarget[il.Name] {
				program = append(program[:i], program[i+1:]...)
				i--
			}
		}
	}
}

func (c *Compiler) passInline(pp *Program) {
	// pre:           post:
	// fn lambda1:    DIVIDE
	//   DIVIDE
	//   RETURN
	//
	program := *pp
	defer func() { *pp = program }()

	// jumps are annoying, so only inline simple functions for now
	// TODO: should always inline if all args are constants
	canInline := func(fn Program) bool {
		if len(fn) > 20 {
			return false
		}
		for _, in := range fn {
			switch in.(type) {
			case instTarget, instJump, instJumpEq:
				return false
			}
		}
		return true
	}
	for i := 0; i+1 < len(program); i++ {
		if ic, ok := program[i].(instConstant); ok {
			if vf, ok := ic.Value.(ValFunc); ok {
				if ic, ok := program[i+1].(instCall); ok && ic.Arity == vf.Arity {
					if cf, ok := c.fns[vf.Name]; ok && canInline(cf.Body) {
						body := cf.Body[1 : len(cf.Body)-1] // strip funcdef and return
						program = append(append(program[:i:i], body...), program[i+2:]...)
					}
				}
			}
		}
	}
}

func (c *Compiler) passLoadConstants() {
	// pre:         post:
	// CONST 3      CONST 3
	// ASSIGN x
	// LOAD x

	fnNames := make([]string, 0, len(c.fns))
	for name := range c.fns {
		fnNames = append(fnNames, name)
	}
	sort.Strings(fnNames)
	var out Program
	for _, name := range fnNames {
		out = append(out, c.fns[name].Body...)
	}
	out = append(out, c.program...)
	old := out.String()
	defer func() {
		out = out[:0]
		for _, name := range fnNames {
			out = append(out, c.fns[name].Body...)
		}
		out = append(out, c.program...)
		new := out.String()
		if old != new {
			c.optDiffs = append(c.optDiffs, optDiff{
				desc:  "load-constants",
				patch: lineDiff(old, new),
			})
		}
	}()

	fns := []*Program{&c.program}
	for _, cf := range c.fns {
		fns = append(fns, &cf.Body)
	}
	consts := make(map[string]instConstant)
	for _, pp := range fns {
		program := *pp
		for i := 0; i+1 < len(program); i++ {
			if ic, ok := program[i].(instConstant); ok {
				if ia, ok := program[i+1].(instAssign); ok {
					consts[ia.Name] = ic
					program = append(program[:i], program[i+2:]...)
					i--
				}
			}
		}
		*pp = program
	}
	for _, pp := range fns {
		program := *pp
		for i := range program {
			if il, ok := program[i].(instLoad); ok {
				if ic, ok := consts[il.Name]; ok {
					program[i] = ic
				}
			}
		}
		*pp = program
	}
}

func (c *Compiler) passUnusedFunctions() {
	fns := []Program{c.program}
	for _, cf := range c.fns {
		fns = append(fns, cf.Body)
	}
	called := make(map[string]bool)
	for _, program := range fns {
		for i := 0; i < len(program); i++ {
			if ic, ok := program[i].(instConstant); ok {
				if vf, ok := ic.Value.(ValFunc); ok {
					called[vf.Name] = true
				}
			}
		}
	}
	for fn, cf := range c.fns {
		if !called[fn] {
			delete(c.fns, fn)
			c.optDiffs = append(c.optDiffs, optDiff{
				desc:  "unused functions",
				patch: lineDiff(cf.Body.String(), ""),
			})
		}
	}
}

type optDiff struct {
	desc  string
	patch string
}

func lineDiff(old, new string) string {
	a := strings.Split(old, "\n")
	b := strings.Split(new, "\n")
	diffs := patience.Diff(a, b)
	s := make([]string, len(diffs))
	for i, d := range diffs {
		switch d.Type {
		case patience.Insert:
			s[i] = "\x1b[32m" + d.Text + "\x1b[0m"
		case patience.Delete:
			s[i] = "\x1b[31m" + d.Text + "\x1b[0m"
		case patience.Equal:
			s[i] = d.Text
		}
	}
	return strings.Join(s, "\n")
}

func (c *Compiler) optimize() {
	c.optDiffs = nil
	opts := []struct {
		name  string
		apply func(*Program)
	}{
		{"fold-constants", c.passFoldConstants},
		{"store-load", c.passStoreLoad},
		{"dedup", c.passDedup},
		{"dead-code", c.passDeadCode},
		{"unused-labels", c.passUnusedLabels},
		{"inline", c.passInline},
	}
	var done bool
	var pass int
	optimizeFn := func(fn *Program) {
		for {
			old := fn.String() // TODO: very inefficient! switch to a dirty bit
			for _, opt := range opts {
				old := fn.String()
				opt.apply(fn)
				if new := fn.String(); new != old {
					c.optDiffs = append(c.optDiffs, optDiff{
						desc:  fmt.Sprintf("%v (pass %v)", opt.name, pass),
						patch: lineDiff(old, new),
					})
				}
				pass++
			}
			if fn.String() == old {
				break
			}
			done = false
		}
	}
	for !done {
		done = true
		for _, fn := range c.fns {
			optimizeFn(&fn.Body)
		}
		optimizeFn(&c.program)
		c.passLoadConstants()
		c.passUnusedFunctions()
	}
}
