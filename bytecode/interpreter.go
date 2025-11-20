package bytecode

import (
	"fmt"
	"maps"
)

type VM struct {
	output    func(Value)
	stack     []Value
	program   []Instruction
	jmpTab    map[string]int
	callStack []stackFrame
}

type stackFrame struct {
	vars map[string]Value
	ip   int
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

func (vm *VM) pushCallStack(ip int) {
	frame := &vm.callStack[len(vm.callStack)-1]
	vars := make(map[string]Value, len(frame.vars))
	maps.Copy(vars, frame.vars)
	vm.callStack = append(vm.callStack, stackFrame{
		vars: vars,
		ip:   ip,
	})
}

func (vm *VM) popCallStack() {
	vm.callStack = vm.callStack[:len(vm.callStack)-1]
}

func (vm *VM) jump(target string) {
	frame := &vm.callStack[len(vm.callStack)-1]
	var ok bool
	frame.ip, ok = vm.jmpTab[target]
	if !ok {
		panic(fmt.Sprintf("undefined label %v", target))
	}
}

func (vm *VM) executeInstruction() error {
	frame := &vm.callStack[len(vm.callStack)-1]
	i := vm.program[frame.ip]
	frame.ip++

	switch i := i.(type) {
	case instConstant:
		vm.push(i.Value)
	case instArray:
		elems := append([]Value(nil), vm.stack[len(vm.stack)-i.Len:]...)
		vm.stack = vm.stack[:len(vm.stack)-len(elems)]
		vm.push(ValArray(elems))
	case instAssoc:
		elems := append([]Value(nil), vm.stack[len(vm.stack)-i.Len*2:]...)
		vm.stack = vm.stack[:len(vm.stack)-len(elems)]
		vm.push(makeAssoc(elems))
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
		vm.jump(i.Target)
	case instJumpEq:
		if vm.pop() == i.Value {
			vm.jump(i.Target)
		}
	case instTarget, instFuncDef:
		// no-op
	case instCall:
		v := vm.pop()
		fn, ok := v.(ValFunc)
		if !ok {
			return fmt.Errorf("cannot call non-function value: %v", v)
		} else if fn.Arity > i.Arity+len(fn.Applied) {
			// partial application
			fn.Applied = fn.Applied[:len(fn.Applied):len(fn.Applied)]
			for range i.Arity {
				fn.Applied = append(fn.Applied, vm.pop())
			}
			vm.push(fn)
		} else if fn.Arity < i.Arity {
			s := "s"
			if fn.Arity == 1 {
				s = ""
			}
			return fmt.Errorf("%v: expected %v argument%v, got %v", fn.Name, fn.Arity, s, i.Arity)
		} else {
			for len(fn.Applied) < fn.Arity {
				fn.Applied = append(fn.Applied, vm.pop())
			}
			if t, ok := vm.jmpTab[fn.Name]; ok {
				for i := len(fn.Applied) - 1; i >= 0; i-- {
					vm.push(fn.Applied[i])
				}
				vm.pushCallStack(t)
			} else if bf, ok := builtins[fn.Name]; ok {
				vm.push(bf.Fn(vm, fn.Applied))
			} else {
				panic(fmt.Sprintf("undefined function %v", fn.Name))
			}
		}
	case instReturn:
		vm.popCallStack()
	default:
		panic(fmt.Sprintf("unhandled instruction %s", i))
	}
	return nil
}

func (vm *VM) Run(program []Instruction, input string) error {
	vm.program = program
	vm.jmpTab = make(map[string]int)
	vm.callStack = []stackFrame{{
		vars: map[string]Value{"input_0": ValString(input)},
		ip:   0,
	}}

	for ip, inst := range program {
		switch inst := inst.(type) {
		case instTarget:
			vm.jmpTab[inst.Name] = ip + 1
		case instFuncDef:
			vm.jmpTab[inst.Name] = ip + 1
		case instReturn:
			vm.callStack[0].ip = ip + 1 // main begins after the last function definition
		}
	}

	for vm.callStack[len(vm.callStack)-1].ip < len(program) {
		if err := vm.executeInstruction(); err != nil {
			return err
		}
	}
	if len(vm.stack) != 0 {
		panic(fmt.Sprintln("stack not empty at end of program:", vm.stack))
	}
	return nil
}

func (vm *VM) Call(fn ValFunc, args []Value) Value {
	fn.Applied = append(fn.Applied[:len(fn.Applied):len(fn.Applied)], args...)
	if fn.Arity != len(fn.Applied) {
		panic(fmt.Sprintf("%v: expected %v arguments, got %v", fn.Name, fn.Arity, len(fn.Applied)))
	}
	if bf, ok := builtins[fn.Name]; ok {
		return bf.Fn(vm, fn.Applied)
	} else if t, ok := vm.jmpTab[fn.Name]; ok {
		for i := len(fn.Applied) - 1; i >= 0; i-- {
			vm.push(fn.Applied[i])
		}
		vm.pushCallStack(t)
		for len(vm.callStack) > 1 && vm.callStack[len(vm.callStack)-1].ip < len(vm.program) {
			vm.executeInstruction()
		}
		return vm.pop()
	} else {
		panic(fmt.Sprintf("undefined function %v", fn.Name))
	}
}

func NewVM() *VM {
	return &VM{
		output: func(Value) {},
	}
}
