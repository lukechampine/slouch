package bytecode

import (
	"fmt"
)

type VM struct {
	output func(Value)
	stack  []Value
}

func (vm *VM) push(v Value) {
	vm.stack = append(vm.stack, v)
}

func (vm *VM) pop() Value {
	if len(vm.stack) == 0 {
		panic("pop: empty stack")
	}
	v := vm.stack[len(vm.stack)-1]
	vm.stack = vm.stack[:len(vm.stack)-1]
	return v
}

func (vm *VM) Run(program []Instruction, input string) error {
	type stackFrame struct {
		vars map[string]Value
		ip   int
	}
	callStack := []stackFrame{{
		vars: map[string]Value{"input": ValString(input)},
		ip:   0,
	}}
	frame := &callStack[0]
	pushCallStack := func(ip int) {
		vars := make(map[string]Value, len(frame.vars))
		for k, v := range frame.vars {
			vars[k] = v
		}
		callStack = append(callStack, stackFrame{
			vars: vars,
			ip:   ip,
		})
		frame = &callStack[len(callStack)-1]
	}
	popCallStack := func() {
		callStack = callStack[:len(callStack)-1]
		frame = &callStack[len(callStack)-1]
	}

	jmpTab := make(map[string]int)
	for ip, inst := range program {
		switch inst := inst.(type) {
		case instTarget:
			jmpTab[inst.Name] = ip + 1
		case instFuncDef:
			jmpTab[inst.Name] = ip + 1
		case instReturn:
			frame.ip = ip + 1 // main begins after the last function definition
		}
	}
	jump := func(target string) {
		var ok bool
		frame.ip, ok = jmpTab[target]
		if !ok {
			panic(fmt.Sprintf("undefined label %v", target))
		}
	}

	for frame.ip < len(program) {
		i := program[frame.ip]
		frame.ip++
		switch i := i.(type) {
		case instConstant:
			vm.push(i.Value)
		case instArray:
			vm.push(ValArray(vm.stack[len(vm.stack)-i.Len:]))
			vm.stack = vm.stack[:len(vm.stack)-i.Len]
		case instAssign:
			frame.vars[i.Name] = vm.pop()
		case instLoad:
			v, ok := frame.vars[i.Name]
			if !ok {
				panic(fmt.Sprintf("undefined variable %v", i.Name))
			}
			vm.push(v)
		case instPrefixOp:
			vm.push(evalPrefixOp(i.Kind, vm.pop()))
		case instInfixOp:
			b, a := vm.pop(), vm.pop()
			vm.push(evalInfixOp(i.Kind, a, b))
		case instOutput:
			vm.output(vm.pop())
		case instDup:
			vm.push(vm.stack[len(vm.stack)-1])
		case instPop:
			vm.pop()
		case instJump:
			jump(i.Target)
		case instJumpEq:
			if vm.pop() == i.Value {
				jump(i.Target)
			}
		case instTarget, instFuncDef:
			// no-op
		case instCall:
			v := vm.pop()
			fn, ok := v.(ValFunc)
			if !ok {
				return fmt.Errorf("cannot call non-function value: %v", v)
			} else if fn.Arity != i.Arity {
				s := "s"
				if fn.Arity == 1 {
					s = ""
				}
				return fmt.Errorf("%v: expected %v argument%v, got %v", fn.Name, fn.Arity, s, i.Arity)
			}
			if t, ok := jmpTab[fn.Name]; ok {
				pushCallStack(t)
			} else if bf, ok := builtins[fn.Name]; ok {
				args := make([]Value, fn.Arity)
				for i := range args {
					args[len(args)-i-1] = vm.pop()
				}
				vm.push(bf.Fn(args))
			} else {
				panic(fmt.Sprintf("undefined function %v", fn.Name))
			}
		case instReturn:
			popCallStack()
		default:
			panic(fmt.Sprintf("unhandled instruction %s", i))
		}
	}
	if len(vm.stack) != 0 {
		panic(fmt.Sprintln("stack not empty at end of program:", vm.stack))
	}
	return nil
}

func NewVM() *VM {
	return &VM{
		output: func(Value) {},
	}
}
