package main

import "fmt"

// ObjectType represents the type of an object
type ObjectType string

const (
	INTEGER_OBJ  ObjectType = "INTEGER"
	FLOAT_OBJ    ObjectType = "FLOAT"
	BOOLEAN_OBJ  ObjectType = "BOOLEAN"
	STRING_OBJ   ObjectType = "STRING"
	NIL_OBJ      ObjectType = "NIL"
	ERROR_OBJ    ObjectType = "ERROR"
	FUNCTION_OBJ ObjectType = "FUNCTION"
)

// Object represents a value in the interpreter
type Object interface {
	Type() ObjectType
	Inspect() string
}

// Integer represents an integer value
type Integer struct {
	Value int64
}

func (i *Integer) Type() ObjectType { return INTEGER_OBJ }
func (i *Integer) Inspect() string  { return fmt.Sprintf("%d", i.Value) }

// Float represents a float value
type Float struct {
	Value float64
}

func (f *Float) Type() ObjectType { return FLOAT_OBJ }
func (f *Float) Inspect() string  { return fmt.Sprintf("%f", f.Value) }

// Boolean represents a boolean value
type Boolean struct {
	Value bool
}

func (b *Boolean) Type() ObjectType { return BOOLEAN_OBJ }
func (b *Boolean) Inspect() string  { return fmt.Sprintf("%t", b.Value) }

// String represents a string value
type String struct {
	Value string
}

func (s *String) Type() ObjectType { return STRING_OBJ }
func (s *String) Inspect() string  { return s.Value }

// Nil represents a nil value
type Nil struct{}

func (n *Nil) Type() ObjectType { return NIL_OBJ }
func (n *Nil) Inspect() string  { return "nil" }

// Error represents an error
type Error struct {
	Message string
}

func (e *Error) Type() ObjectType { return ERROR_OBJ }
func (e *Error) Inspect() string  { return "ERROR: " + e.Message }

// Variable represents a variable binding with mutability information
type Variable struct {
	Value   Object
	Mutable bool
	Line    int // Line where declared
	Column  int // Column where declared
}

// Environment stores variable bindings
type Environment struct {
	store map[string]*Variable
	outer *Environment
}

// NewEnvironment creates a new environment
func NewEnvironment() *Environment {
	s := make(map[string]*Variable)
	return &Environment{store: s, outer: nil}
}

// NewEnclosedEnvironment creates a new environment with an outer environment
func NewEnclosedEnvironment(outer *Environment) *Environment {
	env := NewEnvironment()
	env.outer = outer
	return env
}

// Get retrieves a variable from the environment
func (e *Environment) Get(name string) (*Variable, bool) {
	obj, ok := e.store[name]
	if !ok && e.outer != nil {
		obj, ok = e.outer.Get(name)
	}
	return obj, ok
}

// Set sets a variable in the environment
func (e *Environment) Set(name string, value Object, mutable bool, line, column int) error {
	// Check if variable already exists
	if existing, exists := e.store[name]; exists {
		// Variable exists, check if it's mutable
		if !existing.Mutable {
			return fmt.Errorf("cannot reassign immutable variable '%s' (declared at line %d, col %d)",
				name, existing.Line, existing.Column)
		}
		// Update mutable variable
		existing.Value = value
		return nil
	}

	// Create new variable binding
	e.store[name] = &Variable{
		Value:   value,
		Mutable: mutable,
		Line:    line,
		Column:  column,
	}
	return nil
}

// GetAllVariables returns all variables in the environment (for var report)
func (e *Environment) GetAllVariables() map[string]*Variable {
	return e.store
}

// Evaluator evaluates the AST
type Evaluator struct {
	env *Environment
}

// NewEvaluator creates a new evaluator
func NewEvaluator() *Evaluator {
	return &Evaluator{
		env: NewEnvironment(),
	}
}

// Eval evaluates a node
func (ev *Evaluator) Eval(node Node) Object {
	switch node := node.(type) {
	// Program
	case *Program:
		return ev.evalProgram(node)

	// Statements
	case *AssignmentStatement:
		return ev.evalAssignmentStatement(node)

	// Expressions
	case *IntegerLiteral:
		return ev.evalIntegerLiteral(node)
	case *FloatLiteral:
		return ev.evalFloatLiteral(node)
	case *StringLiteral:
		return &String{Value: node.Value}
	case *BooleanLiteral:
		return &Boolean{Value: node.Value}
	case *NilLiteral:
		return &Nil{}
	case *Identifier:
		return ev.evalIdentifier(node)
	case *InfixExpression:
		return ev.evalInfixExpression(node)
	case *PrefixExpression:
		return ev.evalPrefixExpression(node)
	case *CallExpression:
		return ev.evalCallExpression(node)
	}

	return nil
}

// evalProgram evaluates a program
func (ev *Evaluator) evalProgram(program *Program) Object {
	var result Object

	for _, statement := range program.Statements {
		result = ev.Eval(statement)

		// Stop evaluation if error
		if result != nil && result.Type() == ERROR_OBJ {
			return result
		}
	}

	return result
}

// evalAssignmentStatement evaluates an assignment statement
func (ev *Evaluator) evalAssignmentStatement(stmt *AssignmentStatement) Object {
	// Evaluate the value expression
	val := ev.Eval(stmt.Value)
	if isError(val) {
		return val
	}

	// Set the variable in the environment
	err := ev.env.Set(stmt.Name.Value, val, stmt.Mutable, stmt.Token.Line, stmt.Token.Column)
	if err != nil {
		return &Error{Message: err.Error()}
	}

	return val
}

// evalIntegerLiteral evaluates an integer literal
func (ev *Evaluator) evalIntegerLiteral(node *IntegerLiteral) Object {
	// Parse the integer
	var value int64
	fmt.Sscanf(node.Value, "%d", &value)
	return &Integer{Value: value}
}

// evalFloatLiteral evaluates a float literal
func (ev *Evaluator) evalFloatLiteral(node *FloatLiteral) Object {
	// Parse the float
	var value float64
	fmt.Sscanf(node.Value, "%f", &value)
	return &Float{Value: value}
}

// evalIdentifier evaluates an identifier
func (ev *Evaluator) evalIdentifier(node *Identifier) Object {
	variable, ok := ev.env.Get(node.Value)
	if !ok {
		return &Error{Message: fmt.Sprintf("identifier not found: %s", node.Value)}
	}
	return variable.Value
}

// evalInfixExpression evaluates an infix expression
func (ev *Evaluator) evalInfixExpression(node *InfixExpression) Object {
	left := ev.Eval(node.Left)
	if isError(left) {
		return left
	}

	right := ev.Eval(node.Right)
	if isError(right) {
		return right
	}

	// Handle integer operations
	if left.Type() == INTEGER_OBJ && right.Type() == INTEGER_OBJ {
		return ev.evalIntegerInfixExpression(node.Operator, left.(*Integer), right.(*Integer))
	}

	// Handle float operations
	if left.Type() == FLOAT_OBJ || right.Type() == FLOAT_OBJ {
		return ev.evalFloatInfixExpression(node.Operator, left, right)
	}

	// Handle string concatenation
	if node.Operator == ".." {
		return &String{Value: left.Inspect() + right.Inspect()}
	}

	// Handle boolean operations
	if left.Type() == BOOLEAN_OBJ && right.Type() == BOOLEAN_OBJ {
		return ev.evalBooleanInfixExpression(node.Operator, left.(*Boolean), right.(*Boolean))
	}

	// Handle comparison operations
	switch node.Operator {
	case "==":
		return &Boolean{Value: left.Inspect() == right.Inspect()}
	case "~=", "!=":
		return &Boolean{Value: left.Inspect() != right.Inspect()}
	}

	return &Error{Message: fmt.Sprintf("type mismatch: %s %s %s", left.Type(), node.Operator, right.Type())}
}

// evalIntegerInfixExpression evaluates integer infix expressions
func (ev *Evaluator) evalIntegerInfixExpression(operator string, left, right *Integer) Object {
	leftVal := left.Value
	rightVal := right.Value

	switch operator {
	case "+":
		return &Integer{Value: leftVal + rightVal}
	case "-":
		return &Integer{Value: leftVal - rightVal}
	case "*":
		return &Integer{Value: leftVal * rightVal}
	case "/":
		if rightVal == 0 {
			return &Error{Message: "division by zero"}
		}
		return &Integer{Value: leftVal / rightVal}
	case "%":
		return &Integer{Value: leftVal % rightVal}
	case "<":
		return &Boolean{Value: leftVal < rightVal}
	case "<=":
		return &Boolean{Value: leftVal <= rightVal}
	case ">":
		return &Boolean{Value: leftVal > rightVal}
	case ">=":
		return &Boolean{Value: leftVal >= rightVal}
	case "==":
		return &Boolean{Value: leftVal == rightVal}
	case "~=", "!=":
		return &Boolean{Value: leftVal != rightVal}
	default:
		return &Error{Message: fmt.Sprintf("unknown operator: %s %s %s", INTEGER_OBJ, operator, INTEGER_OBJ)}
	}
}

// evalFloatInfixExpression evaluates float infix expressions
func (ev *Evaluator) evalFloatInfixExpression(operator string, left, right Object) Object {
	var leftVal, rightVal float64

	switch left.Type() {
	case INTEGER_OBJ:
		leftVal = float64(left.(*Integer).Value)
	case FLOAT_OBJ:
		leftVal = left.(*Float).Value
	default:
		return &Error{Message: fmt.Sprintf("invalid type for float operation: %s", left.Type())}
	}

	switch right.Type() {
	case INTEGER_OBJ:
		rightVal = float64(right.(*Integer).Value)
	case FLOAT_OBJ:
		rightVal = right.(*Float).Value
	default:
		return &Error{Message: fmt.Sprintf("invalid type for float operation: %s", right.Type())}
	}

	switch operator {
	case "+":
		return &Float{Value: leftVal + rightVal}
	case "-":
		return &Float{Value: leftVal - rightVal}
	case "*":
		return &Float{Value: leftVal * rightVal}
	case "/":
		if rightVal == 0 {
			return &Error{Message: "division by zero"}
		}
		return &Float{Value: leftVal / rightVal}
	case "<":
		return &Boolean{Value: leftVal < rightVal}
	case "<=":
		return &Boolean{Value: leftVal <= rightVal}
	case ">":
		return &Boolean{Value: leftVal > rightVal}
	case ">=":
		return &Boolean{Value: leftVal >= rightVal}
	case "==":
		return &Boolean{Value: leftVal == rightVal}
	case "~=", "!=":
		return &Boolean{Value: leftVal != rightVal}
	default:
		return &Error{Message: fmt.Sprintf("unknown operator: FLOAT %s FLOAT", operator)}
	}
}

// evalBooleanInfixExpression evaluates boolean infix expressions
func (ev *Evaluator) evalBooleanInfixExpression(operator string, left, right *Boolean) Object {
	switch operator {
	case "and":
		return &Boolean{Value: left.Value && right.Value}
	case "or":
		return &Boolean{Value: left.Value || right.Value}
	case "==":
		return &Boolean{Value: left.Value == right.Value}
	case "~=", "!=":
		return &Boolean{Value: left.Value != right.Value}
	default:
		return &Error{Message: fmt.Sprintf("unknown operator: %s %s %s", BOOLEAN_OBJ, operator, BOOLEAN_OBJ)}
	}
}

// evalPrefixExpression evaluates a prefix expression
func (ev *Evaluator) evalPrefixExpression(node *PrefixExpression) Object {
	right := ev.Eval(node.Right)
	if isError(right) {
		return right
	}

	switch node.Operator {
	case "-":
		return ev.evalMinusPrefixOperator(right)
	case "not":
		return ev.evalNotPrefixOperator(right)
	case "#":
		return ev.evalLengthPrefixOperator(right)
	default:
		return &Error{Message: fmt.Sprintf("unknown operator: %s%s", node.Operator, right.Type())}
	}
}

// evalMinusPrefixOperator evaluates the minus prefix operator
func (ev *Evaluator) evalMinusPrefixOperator(right Object) Object {
	switch right.Type() {
	case INTEGER_OBJ:
		return &Integer{Value: -right.(*Integer).Value}
	case FLOAT_OBJ:
		return &Float{Value: -right.(*Float).Value}
	default:
		return &Error{Message: fmt.Sprintf("unknown operator: -%s", right.Type())}
	}
}

// evalNotPrefixOperator evaluates the not prefix operator
func (ev *Evaluator) evalNotPrefixOperator(right Object) Object {
	switch right.Type() {
	case BOOLEAN_OBJ:
		return &Boolean{Value: !right.(*Boolean).Value}
	case NIL_OBJ:
		return &Boolean{Value: true}
	default:
		return &Boolean{Value: false}
	}
}

// evalLengthPrefixOperator evaluates the length prefix operator (#)
func (ev *Evaluator) evalLengthPrefixOperator(right Object) Object {
	switch right.Type() {
	case STRING_OBJ:
		return &Integer{Value: int64(len(right.(*String).Value))}
	default:
		return &Error{Message: fmt.Sprintf("unknown operator: #%s", right.Type())}
	}
}

// evalCallExpression evaluates a call expression
func (ev *Evaluator) evalCallExpression(node *CallExpression) Object {
	// For now, we'll handle built-in functions like make and free
	// In the future, user-defined functions will be added

	function := node.Function
	if ident, ok := function.(*Identifier); ok {
		switch ident.Value {
		case "make":
			return &Error{Message: "make() not yet implemented - coming in next feature!"}
		case "free":
			return &Error{Message: "free() not yet implemented - coming in next feature!"}
		case "print":
			return ev.evalPrintFunction(node.Arguments)
		default:
			return &Error{Message: fmt.Sprintf("undefined function: %s", ident.Value)}
		}
	}

	return &Error{Message: "not a function"}
}

// evalPrintFunction evaluates the built-in print function
func (ev *Evaluator) evalPrintFunction(args []Expression) Object {
	for i, arg := range args {
		val := ev.Eval(arg)
		if isError(val) {
			return val
		}
		if i > 0 {
			fmt.Print("\t")
		}
		fmt.Print(val.Inspect())
	}
	fmt.Println()
	return &Nil{}
}

// isError checks if an object is an error
func isError(obj Object) bool {
	if obj != nil {
		return obj.Type() == ERROR_OBJ
	}
	return false
}

// GetEnvironment returns the current environment (for testing/reporting)
func (ev *Evaluator) GetEnvironment() *Environment {
	return ev.env
}
