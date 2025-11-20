package bytecode

import (
	"errors"
	"fmt"
	"sort"
	"strconv"

	"lukechampine.com/slouch/ast"
	"lukechampine.com/slouch/token"
)

var opNameTab = map[token.Kind]string{
	token.Negative:  "NEGATE",
	token.Plus:      "ADD",
	token.Minus:     "SUBTRACT",
	token.Star:      "MULTIPLY",
	token.Slash:     "DIVIDE",
	token.And:       "AND",
	token.Or:        "OR",
	token.Equals:    "EQ",
	token.NotEquals: "NEQ",
}

type Instruction interface {
	isInstruction()
	fmt.Stringer
}

type (
	instConstant struct{ Value Value }
	instArray    struct{ Len int }
	instAssoc    struct{ Len int }
	instAssign   struct{ Name string }
	instLoad     struct{ Name string }
	instPrefixOp struct{ Kind token.Kind }
	instInfixOp  struct{ Kind token.Kind }
	instOutput   struct{}
	instDup      struct{}
	instPop      struct{}
	instJump     struct{ Target string }
	instJumpEq   struct {
		Value  Value
		Target string
	}
	instTarget  struct{ Name string }
	instFuncDef struct{ Name string }
	instCall    struct{ Arity int }
	instReturn  struct{}
)

func (instConstant) isInstruction()   {}
func (instArray) isInstruction()      {}
func (instAssoc) isInstruction()      {}
func (instAssign) isInstruction()     {}
func (instLoad) isInstruction()       {}
func (instPrefixOp) isInstruction()   {}
func (instInfixOp) isInstruction()    {}
func (instOutput) isInstruction()     {}
func (instDup) isInstruction()        {}
func (instPop) isInstruction()        {}
func (instJump) isInstruction()       {}
func (instJumpEq) isInstruction()     {}
func (instTarget) isInstruction()     {}
func (instFuncDef) isInstruction()    {}
func (instCall) isInstruction()       {}
func (instReturn) isInstruction()     {}
func (i instConstant) String() string { return fmt.Sprintf("CONST %v", i.Value) }
func (i instArray) String() string    { return fmt.Sprintf("ARRAY %v", i.Len) }
func (i instAssoc) String() string    { return fmt.Sprintf("ASSOC %v", i.Len) }
func (i instAssign) String() string   { return fmt.Sprintf("ASSIGN %s", i.Name) }
func (i instLoad) String() string     { return fmt.Sprintf("LOAD %s", i.Name) }
func (i instPrefixOp) String() string { return opNameTab[i.Kind] }
func (i instInfixOp) String() string  { return opNameTab[i.Kind] }
func (i instOutput) String() string   { return "OUTPUT" }
func (i instDup) String() string      { return "DUP" }
func (i instPop) String() string      { return "POP" }
func (i instJump) String() string     { return fmt.Sprintf("JUMP %v", i.Target) }
func (i instJumpEq) String() string   { return fmt.Sprintf("JUMP_EQ %v %v", i.Value, i.Target) }
func (i instTarget) String() string   { return fmt.Sprintf("@%s", i.Name) }
func (i instFuncDef) String() string  { return fmt.Sprintf("fn %s:", i.Name) }
func (i instCall) String() string     { return fmt.Sprintf("CALL %v", i.Arity) }
func (i instReturn) String() string   { return "RETURN" }

type Program []Instruction

func (p Program) String() string {
	var s string
	for _, in := range p {
		switch in.(type) {
		case instTarget, instFuncDef:
		default:
			s += "\t"
		}
		s += in.String() + "\n"
	}
	return s
}

type compiledFunc struct {
	Arity int
	Body  Program
}

type Compiler struct {
	program  Program
	label    int
	lambda   int
	fns      map[string]*compiledFunc
	vars     map[string]string
	ssa      int
	optDiffs []optDiff
	err      error // sticky
}

func (c *Compiler) setErr(err error) {
	if c.err == nil {
		c.err = err
	}
}

func (c *Compiler) newLabel() string {
	c.label++
	return fmt.Sprintf("label%d", c.label)
}

func (c *Compiler) newLambda() string {
	c.lambda++
	return fmt.Sprintf("lambda%d", c.lambda)
}

func (c *Compiler) emit(i Instruction) {
	if c.err != nil {
		return
	}
	c.program = append(c.program, i)
}

func (c *Compiler) pushScope() func() {
	parent := c.program
	c.program = nil
	vars := c.vars
	c.vars = make(map[string]string)
	for k, v := range vars {
		c.vars[k] = v
	}
	return func() {
		c.program = parent
		c.vars = vars
	}
}

func (c *Compiler) assign(name string) {
	c.vars[name] = fmt.Sprintf("%v_%v", name, c.ssa)
	c.ssa++
	c.emit(instAssign{Name: c.vars[name]})
}

func isHole(n ast.Node) bool {
	_, ok := n.(ast.Hole)
	return ok || n == nil
}

func containsShallowHoles(e ast.Expr) bool {
	isShallow := true
	hasHoles := false
	ast.Visit(e, func(n ast.Node) bool {
		hasHoles = hasHoles || isHole(n)
		switch n.(type) {
		case ast.Lambda, ast.FnCall, ast.Pipe:
			isShallow = false
		}
		return isShallow
	})
	return hasHoles && isShallow
}

func (c *Compiler) emitHoleLambda(e ast.Expr) {
	// create lambda whose body is e, with holes replaced by params
	//
	// NOTE: even though we use x,y,z etc. here, we won't collide with the
	// parent scope, as the compiler immediately rewrites them to SSA form
	// anyway.
	var params []string
	var rec func(ast.Expr) ast.Expr
	rec = func(n ast.Expr) ast.Expr {
		if isHole(n) {
			params = append(params, fmt.Sprintf("hole%v", len(params)))
			return ast.Ident{Name: params[len(params)-1]}
		}
		switch n := n.(type) {
		case ast.Negative:
			n.Value = rec(n.Value)
			return n
		case ast.InfixOp:
			n.Left = rec(n.Left)
			n.Right = rec(n.Right)
			return n
		case ast.Array:
			n.Elems = append([]ast.Expr(nil), n.Elems...)
			for i := range n.Elems {
				n.Elems[i] = rec(n.Elems[i])
			}
			return n
		case ast.Pipe:
			n.Left = rec(n.Left)
			n.Right = rec(n.Right)
			return n
		default:
			return n
		}
	}
	e = rec(e)

	popScope := c.pushScope()
	name := c.newLambda()
	c.emit(instFuncDef{Name: name})
	for i := range params {
		c.assign(params[len(params)-i-1])
	}
	c.pushExpr(e)
	c.emit(instReturn{})
	c.fns[name] = &compiledFunc{
		Arity: len(params),
		Body:  c.program,
	}
	popScope()
	c.emit(instConstant{ValFunc{Name: name, Arity: len(params)}})
}

func (c *Compiler) pushExpr(e ast.Expr) {
	if c.err != nil {
		return
	}
	if containsShallowHoles(e) {
		c.emitHoleLambda(e)
		return
	}
	switch e := e.(type) {
	case ast.Integer:
		i, _ := strconv.ParseInt(e.Value, 10, 64)
		c.emit(instConstant{ValInt(i)})
	case ast.String:
		c.emit(instConstant{ValString(e.Value)})
	case ast.Ident:
		if e.Name == "true" || e.Name == "false" {
			c.emit(instConstant{ValBool(e.Name == "true")})
		} else if ssa, ok := c.vars[e.Name]; ok {
			c.emit(instLoad{ssa})
		} else if cf, ok := c.fns[e.Name]; ok {
			c.emit(instConstant{ValFunc{Name: e.Name, Arity: cf.Arity}})
		} else if bf, ok := builtins[e.Name]; ok {
			c.emit(instConstant{ValFunc{Name: e.Name, Arity: bf.Arity}})
		} else {
			c.setErr(fmt.Errorf("undefined: %v", e.Name))
		}
	case ast.Array:
		if e.Assoc && len(e.Elems)%2 != 0 {
			c.setErr(errors.New("assoc: dangling key"))
			return
		}
		for _, e := range e.Elems {
			c.pushExpr(e)
		}
		if e.Assoc {
			c.emit(instAssoc{Len: len(e.Elems) / 2})
		} else {
			c.emit(instArray{Len: len(e.Elems)})
		}
	case ast.Negative:
		c.pushExpr(e.Value)
		c.emit(instPrefixOp{Kind: token.Negative})
	case ast.InfixOp:
		if e.Op == token.And || e.Op == token.Or {
			// short circuit
			c.pushExpr(e.Left)
			c.emit(instDup{})
			l := c.newLabel()
			c.emit(instJumpEq{Value: ValBool(e.Op == token.Or), Target: l})
			c.emit(instPop{})
			c.pushExpr(e.Right)
			c.emit(instTarget{Name: l})
		} else {
			c.pushExpr(e.Left)
			c.pushExpr(e.Right)
			c.emit(instInfixOp{Kind: e.Op})
		}
	case ast.Lambda:
		popScope := c.pushScope()
		name := c.newLambda()
		c.emit(instFuncDef{Name: name})
		params := e.Params()
		for i := range params {
			c.assign(params[len(params)-i-1])
		}
		c.pushExpr(e.Body)
		c.emit(instReturn{})
		c.fns[name] = &compiledFunc{
			Arity: e.NumArgs(),
			Body:  c.program,
		}
		popScope()
		c.emit(instConstant{ValFunc{Name: name, Arity: e.NumArgs()}})
	case ast.FnCall:
		for i := range e.Args {
			c.pushExpr(e.Args[len(e.Args)-i-1])
		}
		c.pushExpr(e.Fn)
		c.emit(instCall{Arity: len(e.Args)})
	// case ast.Rep:
	// 	popScope := c.pushScope()
	// 	name := c.newLambda()
	// 	c.emit(instFuncDef{Name: name})
	// 	params := e.Params()
	// 	for i := range params {
	// 		c.assign(params[len(params)-i-1])
	// 	}
	// 	c.pushExpr(e.Body)
	// 	c.emit(instReturn{})
	// 	c.fns[name] = &compiledFunc{
	// 		Arity: e.NumArgs(),
	// 		Body:  c.program,
	// 	}
	// 	popScope()
	// 	c.emit(instConstant{ValFunc{Name: name, Arity: 1}})

	case ast.Pipe:
		switch e.Token.Kind {
		case token.Pipe:
			c.pushExpr(ast.FnCall{Fn: e.Right, Args: []ast.Expr{e.Left}})
		case token.PipeRep:
			c.pushExpr(ast.FnCall{Fn: ast.Rep{Fn: e.Right}, Args: []ast.Expr{e.Left}})
		case token.PipeSplat:
			c.pushExpr(ast.FnCall{Fn: ast.Splat{Fn: e.Right}, Args: []ast.Expr{e.Left}})
		default:
			panic("unhandled pipe type")
		}
	default:
		panic(fmt.Sprintf("unhandled expr type %T", e))
	}
}

func (c *Compiler) Compile(p ast.Program) (Program, error) {
	c.emit(instFuncDef{Name: "main"})
	for _, stmt := range p.Stmts {
		switch stmt := stmt.(type) {
		case ast.AssignStmt:
			// to support recursion, we need to assign the name before
			// evaluating the body
			c.vars[stmt.Name.Name] = fmt.Sprintf("%v_%v", stmt.Name.Name, c.ssa)
			c.ssa++
			c.pushExpr(stmt.X)
			c.emit(instAssign{Name: c.vars[stmt.Name.Name]})
		case ast.ExprStmt:
			c.pushExpr(stmt.X)
			c.emit(instOutput{})
		}
	}
	if c.err != nil {
		return nil, c.err
	}

	c.optimize()

	// concat function definitions
	fns := make([]string, 0, len(c.fns))
	for name := range c.fns {
		fns = append(fns, name)
	}
	sort.Strings(fns)
	var out Program
	for _, name := range fns {
		out = append(out, c.fns[name].Body...)
	}
	c.program = append(out, c.program...)

	return c.program, nil
}

func NewCompiler() *Compiler {
	c := &Compiler{
		fns:  make(map[string]*compiledFunc),
		vars: make(map[string]string),
	}
	c.vars["input"] = fmt.Sprintf("%v_%v", "input", c.ssa)
	c.ssa++
	return c
}
