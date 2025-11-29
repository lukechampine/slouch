package bytecode

import (
	"fmt"
	"slices"
	"strings"

	"github.com/peter-evans/patience"
	"lukechampine.com/slouch/token"
)

func stackRequired(i Instruction) int {
	switch i := i.(type) {
	case instArray:
		return i.Len
	case instAssoc:
		return i.Len
	case instDynamicCall:
		return i.Arity + 1
	case instStaticCall:
		return i.Func.Need()
	case instInfixOp, instSwap:
		return 2
	case instAssign, instPrefixOp, instSplat, instRep, instOutput, instDup, instPop, instTruthy, instJumpEq:
		return 1
	case instTarget, instFuncDef, instReturn, instJump, instConstant, instLoad:
		return 0
	default:
		panic(fmt.Sprintf("unhandled instruction %T", i))
	}
}

func stackImpact(is ...Instruction) (total int) {
	for _, i := range is {
		switch i := i.(type) {
		case instArray:
			total += 1 - i.Len
		case instAssoc:
			total += 1 - i.Len
		case instDynamicCall:
			total += 1 - (i.Arity + 1)
		case instStaticCall:
			total += 1 - (i.Func.Arity - len(i.Func.Applied))
		case instInfixOp, instAssign, instPrefixOp, instOutput, instPop, instJumpEq:
			total += -1
		case instTarget, instFuncDef, instReturn, instSwap, instSplat, instRep, instTruthy, instJump:
			total += 0
		case instConstant, instLoad, instDup:
			total += 1
		default:
			panic(fmt.Sprintf("unhandled instruction %T", i))
		}
	}
	return
}

func match1[A Instruction](p Program, a *A) bool {
	if ia, ok := p[0].(A); ok {
		*a = ia
		return true
	}
	return false
}

func match2[A, B Instruction](p Program, a *A, b *B) bool {
	return match1(p, a) && match1(p[1:], b)
}

func match3[A, B, C Instruction](p Program, a *A, b *B, c *C) bool {
	return match2(p, a, b) && match1(p[2:], c)
}

func match4[A, B, C, D Instruction](p Program, a *A, b *B, c *C, d *D) bool {
	return match3(p, a, b, c) && match1(p[3:], d)
}

func (c *Compiler) passFoldConstants(pp *Program) {
	// pre:         post:
	// CONST 2      CONST 5
	// CONST 3
	// ADD
	program := *pp
	defer func() { *pp = program }()

	for i := 1; i < len(program); i++ {
		constArgs := func(n int) bool {
			if i-n < 0 {
				return false
			}
			for j := range n {
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
				args[j] = program[i-j-1].(instConstant).Value
			}
			program[i] = instConstant{Value: fn(args)}
			program = append(program[:i-n], program[i:]...)
		}

		if !constArgs(1) {
			continue // minimum requirement for all cases
		}
		switch in := program[i].(type) {
		case instArray:
			replace(in.Len, func(args []Value) Value { return ValArray(args) })
		case instAssoc:
			replace(in.Len*2, func(args []Value) Value { return makeAssoc(args) })
		case instPrefixOp:
			replace(1, func(args []Value) Value { return evalPrefixOp(in.Kind, args[0]) })
		case instTruthy:
			replace(1, func(args []Value) Value { return ValBool(internalTruthy(args[0])) })
		case instInfixOp:
			// TODO: special case for Dot
			replace(2, func(args []Value) Value { return evalInfixOp(in.Kind, args[0], args[1]) })
		case instDup:
			program[i] = program[i-1]
		case instSwap:
			if constArgs(2) {
				program = slices.Replace(program, i-2, i+1, Program{program[i-1], program[i-2]}...)
			}
		case instJumpEq:
			v := program[i-1].(instConstant).Value
			if evalInfixOp(token.Equals, v, in.Value).(ValBool) {
				program[i] = instJump{Target: in.Target}
				program = append(program[:i-1], program[i:]...)
			} else {
				program = append(program[:i-1], program[i+1:]...)
			}
		case instDynamicCall:
			if fn := program[i-1].(instConstant).Value.(ValFunc); fn.Mod == 0 && in.Arity != fn.Need() {
				for len(fn.Applied) < fn.Arity && fn.Mod == 0 && constArgs(2) {
					v := program[i-2].(instConstant).Value
					fn.Applied = append(fn.Applied, v)
					if in.Arity--; in.Arity == 0 {
						program = slices.Replace(program, i-2, i+1, Program{instConstant{Value: fn}}...)
					} else {
						program = slices.Replace(program, i-2, i+1, Program{instConstant{Value: fn}, in}...)
					}
				}
			}
		case instStaticCall:
			for len(in.Func.Applied) < in.Func.Arity && in.Func.Mod == 0 && constArgs(1) {
				v := program[i-1].(instConstant).Value
				in.Func.Applied = append(in.Func.Applied, v)
				program[i] = in
				program = append(program[:i-1], program[i:]...)
			}
			if bfn, ok := builtins[in.Func.Name]; ok && bfn.Comptime && in.Func.Mod == 0 && in.Func.Need() == 0 {
				program[i-1] = instConstant{Value: bfn.Fn(nil, in.Func.Applied)}
			}
		case instSplat:
			if fn, ok := program[i-1].(instConstant).Value.(ValFunc); ok {
				fn.Mod = token.Splat
				replace(1, func(args []Value) Value { return fn })
			}
		case instRep:
			if fn, ok := program[i-1].(instConstant).Value.(ValFunc); ok {
				fn.Mod = token.Rep
				replace(1, func(args []Value) Value { return fn })
			}
		}
	}
}

func (c *Compiler) passUnsplat(pp *Program) {
	// pre:              post:
	// ARRAY 2           CALL foo(_,_)
	// CALL -<foo(_,_)
	program := *pp
	defer func() { *pp = program }()

	for i := 0; i+1 < len(program); i++ {
		var ia instArray
		var isc instStaticCall
		if match2(program[i:], &ia, &isc) && isc.Func.Mod == token.Splat {
			if isc.Func.Arity == ia.Len {
				isc.Func.Mod = 0
				program = slices.Replace(program, i, i+2, Program{isc}...)
			}
		}
		// also check for array consts
		var ic instConstant
		if match2(program[i:], &ic, &isc) && isc.Func.Mod == token.Splat {
			if va, ok := ic.Value.(ValArray); ok {
				if isc.Func.Arity == len(va) {
					for i := range va {
						isc.Func.Applied = append(isc.Func.Applied, va[len(va)-i-1])
					}
					isc.Func.Mod = 0
					program = slices.Replace(program, i, i+2, Program{isc}...)
				}
			}
		}
	}
}

func (c *Compiler) passUnrep(pp *Program) {
	// pre:              post:
	// CONST 2           CALL foo(2,2)
	// CALL -:foo(_,_)
	program := *pp
	defer func() { *pp = program }()

	for i := 0; i+1 < len(program); i++ {
		var ic instConstant
		var isc instStaticCall
		if match2(program[i:], &ic, &isc) && isc.Func.Mod == token.Rep {
			for isc.Func.Need() > 0 {
				isc.Func.Applied = append(isc.Func.Applied, ic.Value)
			}
			isc.Func.Mod = 0
			program = slices.Replace(program, i, i+2, Program{isc}...)
		}
	}
}

func (c *Compiler) passStaticCall(pp *Program) {
	// pre:             post:
	// CONST foo(_,_)   CALL foo(_,_)
	// DCALL 2
	program := *pp
	defer func() { *pp = program }()

	for i := 0; i+1 < len(program); i++ {
		var ic instConstant
		var idc instDynamicCall
		if match2(program[i:], &ic, &idc) {
			if vf, ok := ic.Value.(ValFunc); ok {
				if idc.Arity == vf.Need() {
					program[i] = instStaticCall{Func: vf}
					program = append(program[:i+1], program[i+2:]...)
					i--
				}
			}
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
	loads := make(map[string]int)
	for i := 0; i < len(c.program); i++ {
		if il, ok := c.program[i].(instLoad); ok {
			loads[il.Name]++
		}
	}
	for _, cf := range c.fns {
		for i := 0; i < len(cf.Body); i++ {
			if il, ok := cf.Body[i].(instLoad); ok {
				loads[il.Name]++
			}
		}
	}

outer:
	for i := 0; i+1 < len(program); i++ {
		if ia, ok := program[i].(instAssign); ok {
			if loads[ia.Name] != 1 {
				continue
			}
			// scan forward, tracking stack height
			var height int
			// HACK: if assign was preceded by a dup (common after CSE),
			// we can allow stack accesses to be one deeper
			dup := 0
			if i > 0 {
				if _, ok := program[i-1].(instDup); ok {
					dup = 1
				}
			}
			for j := i + 1; j < len(program); j++ {
				if il, ok := program[j].(instLoad); ok && il.Name == ia.Name {
					if height == 0 {
						// delete both the load and the store
						program = slices.Delete(program, j, j+1)
						program = slices.Delete(program, i, i+1)
					}
					continue outer
				} else {
					if stackRequired(program[j]) > height+dup {
						// deleting store+load would change behavior
						continue outer
					}
					height += stackImpact(program[j])
				}
			}
		}
	}
}

func (c *Compiler) passDupLoad(pp *Program) {
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

func (c *Compiler) passSwapLoad(pp *Program) {
	// pre:         post:
	// ASSIGN x     SWAP
	// ASSIGN y
	// LOAD x
	// LOAD y
	program := *pp
	defer func() { *pp = program }()

	for i := 0; i+1 < len(program); i++ {
		var ia1, ia2 instAssign
		var il1, il2 instLoad
		if match4(program[i:], &ia1, &ia2, &il1, &il2) &&
			ia1.Name == il1.Name && ia2.Name == il2.Name {
			program = slices.Replace(program, i, i+4, Program{instSwap{}}...)
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
		var ic instConstant
		var ip instPop
		if match2(program[i:], &ic, &ip) {
			program = slices.Delete(program, i, i+2)
		} else if ij, ok := program[i].(instJump); ok {
			for j := i + 1; j < len(program); j++ {
				if il, ok := program[j].(instTarget); ok {
					if ij.Target == il.Name {
						program = slices.Delete(program, i, j)
					} else {
						program = slices.Delete(program, i+1, j)
					}
					continue outer
				}
			}
		}
		var id instDup
		var is instSwap
		if match2(program[i:], &id, &is) {
			program = slices.Delete(program, i, i+2)
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
	// pre:             post:
	// CALL foo(_,_)    DIVIDE
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
	for i := 0; i < len(program); i++ {
		if isc, ok := program[i].(instStaticCall); ok && isc.Func.Mod == 0 {
			if cf, ok := c.fns[isc.Func.Name]; ok && canInline(cf.Body) {
				body := slices.Clone(cf.Body[1 : len(cf.Body)-1]) // strip funcdef and return
				// give all vars unique names
				rename := make(map[string]string)
				for i := range body {
					switch in := body[i].(type) {
					case instAssign:
						name := c.newVar(in.Name + "_inline")
						rename[in.Name] = name
						in.Name = name
						body[i] = in
					case instLoad:
						if newName, ok := rename[in.Name]; ok {
							in.Name = newName
							body[i] = in
						}
					}
				}

				// push applied args onto stack
				args := make([]Instruction, len(isc.Func.Applied))
				for i := range isc.Func.Applied {
					args[i] = instConstant{Value: isc.Func.Applied[i]}
				}
				program = slices.Replace(program, i, i+1, append(args, body...)...)
			}
		}
	}
}

func (c *Compiler) passCSE(pp *Program) {
	// pre:               post:
	// LOAD x_0           LOAD x_0
	// CALL string(_)     CALL string(_)
	// ADD			      DUP
	// LOAD x_0           ASSIGN x_1
	// CALL string(_)     ADD
	//                    LOAD x_1

	program := *pp
	defer func() { *pp = program }()

	worthIt := func(seq Program) bool {
		// must have a total stack impact of +1 and contain at least 1 call
		if stackImpact(seq...) != 1 {
			return false
		}
		for _, in := range seq {
			switch in.(type) {
			case instDynamicCall, instStaticCall:
				return true
			}
		}
		return false
	}

	for i := 0; i < len(program); i++ {
		for size := len(program[i:]) / 2; size > 0 && i+size <= len(program); size-- {
			if !worthIt(program[i : i+size]) {
				continue
			}
			seq := program[i : i+size].String()
			var matches []int
			for j := i + size; j+size <= len(program); j++ {
				if program[j:j+size].String() == seq {
					matches = append(matches, j)
					j += size - 1
				}
			}
			if len(matches) > 0 {
				name := c.newVar("cse")
				for _, j := range matches {
					program = slices.Replace(program, j, j+size, Program{instLoad{Name: name}}...)
				}
				program = slices.Insert(program, i+size, Program{instDup{}, instAssign{Name: name}}...)
			}
		}
	}
}

func (c *Compiler) passLoadConstants() {
	// pre:         post:
	// CONST 3      CONST 3
	// ASSIGN x
	// LOAD x

	fns := []*Program{&c.program}
	for _, cf := range c.fns {
		fns = append(fns, &cf.Body)
	}
	consts := make(map[string]instConstant)
	for _, pp := range fns {
		program := *pp
		for i := 0; i+1 < len(program); i++ {
			var ic instConstant
			var ia instAssign
			if match2(program[i:], &ic, &ia) {
				consts[ia.Name] = ic
				program = append(program[:i], program[i+2:]...)
				i--
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
	called := make(map[string]bool)
	var visit func(v Value)
	visitProgram := func(p Program) {
		for _, in := range p {
			switch in := in.(type) {
			case instConstant:
				visit(in.Value)
			case instStaticCall:
				visit(in.Func)
			}
		}
	}
	visit = func(v Value) {
		if vf, ok := v.(ValFunc); ok {
			if !called[vf.Name] {
				called[vf.Name] = true
				if cf, ok := c.fns[vf.Name]; ok {
					visitProgram(cf.Body)
				}
			}
			for _, v := range vf.Applied {
				visit(v)
			}
		}
	}
	visitProgram(c.program)
	for fn := range c.fns {
		if !called[fn] {
			delete(c.fns, fn)
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
		{"unsplat", c.passUnsplat},
		{"unrep", c.passUnrep},
		{"static-call", c.passStaticCall},
		{"store-load", c.passStoreLoad},
		{"dup-load", c.passDupLoad},
		{"swap-load", c.passSwapLoad},
		{"dead-code", c.passDeadCode},
		{"unused-labels", c.passUnusedLabels},
		{"inline", c.passInline},
		{"cse", c.passCSE},
	}
	wholeOpt := []struct {
		name  string
		apply func()
	}{
		{"load-constants", c.passLoadConstants},
		{"unused-functions", c.passUnusedFunctions},
	}
	var pass int
	optimizeFn := func(fn *Program) {
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
	}

	for {
		old := c.output().String()
		for _, fn := range c.fns {
			optimizeFn(&fn.Body)
		}
		optimizeFn(&c.program)

		for _, opt := range wholeOpt {
			old := c.output().String()
			opt.apply()
			if new := c.output().String(); old != new {
				c.optDiffs = append(c.optDiffs, optDiff{
					desc:  opt.name,
					patch: lineDiff(old, new),
				})
			}
		}
		if c.output().String() == old {
			break
		}
	}
}
