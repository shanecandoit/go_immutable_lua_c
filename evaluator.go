package main

import (
	"crypto/sha256"
	"encoding/hex"
	"fmt"
	"reflect"
	"time"
)

// ObjectType represents the type of an object
type ObjectType string

const (
	INTEGER_OBJ      ObjectType = "INTEGER"
	FLOAT_OBJ        ObjectType = "FLOAT"
	BOOLEAN_OBJ      ObjectType = "BOOLEAN"
	STRING_OBJ       ObjectType = "STRING"
	NIL_OBJ          ObjectType = "NIL"
	ERROR_OBJ        ObjectType = "ERROR"
	FUNCTION_OBJ     ObjectType = "FUNCTION"
	POINTER_OBJ      ObjectType = "POINTER"
	RETURN_VALUE_OBJ ObjectType = "RETURN_VALUE"
	BUILTIN_OBJ      ObjectType = "BUILTIN"
)

// Object represents a value in the interpreter
type Object interface {
	Type() ObjectType
	Inspect() string
}

// ReturnValue wraps a return value
type ReturnValue struct {
	Value Object
}

func (rv *ReturnValue) Type() ObjectType { return RETURN_VALUE_OBJ }
func (rv *ReturnValue) Inspect() string  { return rv.Value.Inspect() }

// Builtin represents a built-in function
type Builtin struct {
	Fn func(ev *Evaluator, args ...Object) Object
}

func (b *Builtin) Type() ObjectType { return BUILTIN_OBJ }
func (b *Builtin) Inspect() string  { return "builtin function" }

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

// Function represents a user-defined function
type Function struct {
	Parameters []*Identifier
	Body       *BlockStatement
	Env        *Environment
}

func (f *Function) Type() ObjectType { return FUNCTION_OBJ }
func (f *Function) Inspect() string {
	return "function"
}

// MemoryTracker tracks all allocated pointers for free report
type MemoryTracker struct {
	allocations map[string]*Pointer
}

func NewMemoryTracker() *MemoryTracker {
	return &MemoryTracker{
		allocations: make(map[string]*Pointer),
	}
}

// Evaluator evaluates the AST
type Evaluator struct {
	env    *Environment
	memory *MemoryTracker
}

// NewEvaluator creates a new evaluator
func NewEvaluator() *Evaluator {
	return &Evaluator{
		env:    NewEnvironment(),
		memory: NewMemoryTracker(),
	}
}

// Eval evaluates a node
func (ev *Evaluator) Eval(node Node) Object {
	if node == nil {
		return &Nil{}
	}
	// Handle typed nil pointers (e.g., var p *AssignmentStatement = nil stored in an interface)
	v := reflect.ValueOf(node)
	if v.Kind() == reflect.Ptr && v.IsNil() {
		return &Nil{}
	}
	switch n := node.(type) {
	case *Program:
		return ev.evalProgram(n)
	case *ExpressionStatement:
		return ev.Eval(n.Expression)
	case *IntegerLiteral:
		return ev.evalIntegerLiteral(n)
	case *FloatLiteral:
		return ev.evalFloatLiteral(n)
	case *StringLiteral:
		return &String{Value: n.Value}
	case *FunctionLiteral:
		// Create function object capturing current environment
		fn := &Function{
			Parameters: n.Parameters,
			Body:       n.Body,
			Env:        ev.env,
		}
		return fn
	case *BooleanLiteral:
		return &Boolean{Value: n.Value}
	case *NilLiteral:
		return &Nil{}
	case *PrefixExpression:
		return ev.evalPrefixExpression(n)
	case *InfixExpression:
		return ev.evalInfixExpression(n)
	case *IfStatement:
		return ev.evalIfStatement(n)
	case *IfExpression:
		return ev.evalIfExpression(n)
	case *BlockStatement:
		return ev.evalBlockStatement(n)
	case *ReturnStatement:
		val := ev.Eval(n.ReturnValue)
		if isError(val) {
			return val
		}
		return &ReturnValue{Value: val}
	case *AssignmentStatement:
		value := ev.Eval(n.Value)
		if isError(value) {
			return value
		}
		err := ev.env.Set(n.Name.String(), value, n.Mutable, n.Token.Line, n.Token.Column)
		if err != nil {
			return &Error{Message: err.Error()}
		}
		return value
	case *Identifier:
		return ev.evalIdentifier(n)
	case *NumericForStatement:
		return ev.evalNumericForStatement(n)
	case *GenericForStatement:
		return ev.evalGenericForStatement(n)
	case *CallExpression:
		return ev.evalCallExpression(n)
	default:
		return &Error{Message: fmt.Sprintf("unhandled node type: %T", n)}
	}
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

// evalNumericForStatement evaluates a numeric for loop
func (ev *Evaluator) evalNumericForStatement(stmt *NumericForStatement) Object {
	// Evaluate start, end, and step
	startObj := ev.Eval(stmt.Start)
	if isError(startObj) {
		return startObj
	}

	endObj := ev.Eval(stmt.End)
	if isError(endObj) {
		return endObj
	}

	// Get integer values
	startInt, ok := startObj.(*Integer)
	if !ok {
		return &Error{Message: "for loop start value must be an integer"}
	}

	endInt, ok := endObj.(*Integer)
	if !ok {
		return &Error{Message: "for loop end value must be an integer"}
	}

	// Get step (default to 1 if not provided)
	step := int64(1)
	if stmt.Step != nil {
		stepObj := ev.Eval(stmt.Step)
		if isError(stepObj) {
			return stepObj
		}
		stepInt, ok := stepObj.(*Integer)
		if !ok {
			return &Error{Message: "for loop step value must be an integer"}
		}
		step = stepInt.Value
	}

	if step == 0 {
		return &Error{Message: "for loop step cannot be zero"}
	}

	// Create new environment for loop scope
	loopEnv := NewEnclosedEnvironment(ev.env)
	prevEnv := ev.env
	ev.env = loopEnv

	var result Object = &Nil{}

	// Create loop variable as immutable
	if err := ev.env.Set(stmt.VarName.Value, &Integer{Value: startInt.Value}, false, stmt.Token.Line, stmt.Token.Column); err != nil {
		return &Error{Message: err.Error()}
	}

	// Execute loop
	i := startInt.Value
	for {
		// Check loop condition based on step direction
		if step > 0 && i > endInt.Value {
			break
		}
		if step < 0 && i < endInt.Value {
			break
		}

		// Update loop variable directly
		ev.env.Update(stmt.VarName.Value, &Integer{Value: i})

		// Execute body
		result = ev.evalBlockStatement(stmt.Body)
		if isError(result) {
			ev.env = prevEnv
			return result
		}

		i += step
	}

	// Restore previous environment
	ev.env = prevEnv

	return result
}

// evalGenericForStatement evaluates a generic for loop
func (ev *Evaluator) evalGenericForStatement(stmt *GenericForStatement) Object {
	// For now, we'll implement a simple range() function support
	// In the future, this can be extended to support custom iterators

	// Evaluate iterator expression
	iteratorObj := ev.Eval(stmt.Iterator)
	if isError(iteratorObj) {
		return iteratorObj
	}

	// Check if it's a call to range() or similar iterator function
	// For simplicity, we'll just return an error for now
	// This is a placeholder for future iterator implementation
	return &Error{Message: "generic for loops with iterators not yet implemented"}
}

// evalBlockStatement evaluates a block statement
func (ev *Evaluator) evalBlockStatement(block *BlockStatement) Object {
	var result Object

	for _, statement := range block.Statements {
		result = ev.Eval(statement)

		if isError(result) {
			return result
		}
		// Propagate return values immediately
		if result != nil && result.Type() == RETURN_VALUE_OBJ {
			return result
		}
	}

	if result == nil {
		return &Nil{}
	}

	return result
}

// isTruthy determines if a value is truthy
// In Lua: nil and false are falsy, everything else is truthy
func isTruthy(obj Object) bool {
	switch obj := obj.(type) {
	case *Nil:
		return false
	case *Boolean:
		return obj.Value
	default:
		return true
	}
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
	if right.Type() == FLOAT_OBJ {
		return &Float{Value: -right.(*Float).Value}
	}

	switch right.Type() {
	case INTEGER_OBJ:
		return &Integer{Value: -right.(*Integer).Value}
	default:
		return &Error{Message: fmt.Sprintf("unknown operator: -%s", right.Type())}
	}
}

// evalNotPrefixOperator evaluates the not prefix operator
func (ev *Evaluator) evalNotPrefixOperator(right Object) Object {
	if right == nil {
		return &Boolean{Value: true}
	}

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
	// If the function expression is a bare identifier, handle builtin names first
	if ident, ok := node.Function.(*Identifier); ok {
		switch ident.Value {
		case "make":
			return ev.evalMakeFunction(node)
		case "free":
			return ev.evalFreeFunction(node)
		case "print":
			return ev.evalPrintFunction(node.Arguments)
		}
	}

	// Otherwise evaluate the function expression
	fnObj := ev.Eval(node.Function)
	if isError(fnObj) {
		return fnObj
	}

	// Handle user-defined functions and builtin objects
	switch fn := fnObj.(type) {
	case *Function:
		// Evaluate arguments
		args := []Object{}
		for _, a := range node.Arguments {
			evaluated := ev.Eval(a)
			if isError(evaluated) {
				return evaluated
			}
			args = append(args, evaluated)
		}

		// Create new environment enclosed by function's environment
		extendedEnv := NewEnclosedEnvironment(fn.Env)
		// Bind parameters
		for i, param := range fn.Parameters {
			var val Object = &Nil{}
			if i < len(args) {
				val = args[i]
			}
			// Parameters are local immutable bindings
			extendedEnv.Set(param.Value, val, false, node.Token.Line, node.Token.Column)
		}

		// Save current env and switch
		prevEnv := ev.env
		ev.env = extendedEnv

		// Evaluate function body
		result := ev.evalBlockStatement(fn.Body)

		// Restore environment
		ev.env = prevEnv

		// Unwrap return value
		if rv, ok := result.(*ReturnValue); ok {
			return rv.Value
		}
		return result
	case *Builtin:
		// Evaluate args as Objects
		args := []Object{}
		for _, a := range node.Arguments {
			evaluated := ev.Eval(a)
			if isError(evaluated) {
				return evaluated
			}
			args = append(args, evaluated)
		}
		return fn.Fn(ev, args...)
	default:
		return &Error{Message: "not a function"}
	}
}

// evalMakeFunction evaluates the built-in make function
// Syntax: make(type, size) returns a SHA256 pointer
func (ev *Evaluator) evalMakeFunction(node *CallExpression) Object {
	if len(node.Arguments) != 2 {
		return &Error{Message: "make() requires 2 arguments: make(type, size)"}
	}

	// Get type name - accept either identifier or string literal
	var typeName string
	switch t := node.Arguments[0].(type) {
	case *Identifier:
		typeName = t.Value
	case *StringLiteral:
		typeName = t.Value
	default:
		return &Error{Message: "make() first argument must be a type name"}
	}

	// Get size
	sizeObj := ev.Eval(node.Arguments[1])
	if isError(sizeObj) {
		return sizeObj
	}

	var size int64
	if intObj, ok := sizeObj.(*Integer); ok {
		size = intObj.Value
	} else {
		return &Error{Message: "make() second argument must be an integer"}
	}

	if size <= 0 {
		return &Error{Message: "make() size must be positive"}
	}

	// Generate SHA256 hash from allocation info
	hash := ev.generatePointerHash(typeName, size, node.Token.Line, node.Token.Column)

	// Create pointer object
	pointer := &Pointer{
		Hash:     hash,
		TypeName: typeName,
		Size:     size,
		Freed:    false,
		Metadata: &PointerMetadata{
			AllocLine:   node.Token.Line,
			AllocColumn: node.Token.Column,
			AllocTime:   time.Now(),
		},
	}

	// Track allocation
	ev.memory.allocations[hash] = pointer

	return pointer
}

// evalFreeFunction evaluates the built-in free function
// Syntax: free(pointer)
func (ev *Evaluator) evalFreeFunction(node *CallExpression) Object {
	if len(node.Arguments) != 1 {
		return &Error{Message: "free() requires 1 argument: free(pointer)"}
	}

	// Evaluate the argument
	arg := ev.Eval(node.Arguments[0])
	if isError(arg) {
		return arg
	}

	// Check if it's a pointer
	pointer, ok := arg.(*Pointer)
	if !ok {
		return &Error{Message: fmt.Sprintf("free() requires a pointer, got %s", arg.Type())}
	}

	// Check if already freed
	if pointer.Freed {
		return &Error{Message: fmt.Sprintf("double free detected for pointer %s", pointer.Hash[:16])}
	}

	// Mark as freed in the memory tracker's allocation map
	if trackedPtr, exists := ev.memory.allocations[pointer.Hash]; exists {
		trackedPtr.Freed = true

		// Update metadata
		if trackedPtr.Metadata.LastWrite == nil {
			trackedPtr.Metadata.LastWrite = &AccessInfo{
				Line:   node.Token.Line,
				Column: node.Token.Column,
				Time:   time.Now(),
			}
		}
	}

	// Also mark the local pointer as freed
	pointer.Freed = true

	return &Nil{}
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

// GetMemoryTracker returns the memory tracker (for reporting)
func (ev *Evaluator) GetMemoryTracker() *MemoryTracker {
	return ev.memory
}

// generatePointerHash generates a SHA256 hash for a pointer allocation
func (ev *Evaluator) generatePointerHash(typeName string, size int64, line, column int) string {
	// Create a unique string from allocation details
	data := fmt.Sprintf("%s:%d:%d:%d:%d", typeName, size, line, column, time.Now().UnixNano())

	// Generate SHA256 hash
	hash := sha256.Sum256([]byte(data))
	return hex.EncodeToString(hash[:])
}

// GetFreeReport generates a memory leak report
func (ev *Evaluator) GetFreeReport() string {
	var report string
	report += "=== Memory Free Report ===\n"

	var leaked []*Pointer
	totalAllocated := 0
	totalFreed := 0

	for _, ptr := range ev.memory.allocations {
		totalAllocated++
		if ptr.Freed {
			totalFreed++
		} else {
			leaked = append(leaked, ptr)
		}
	}

	report += fmt.Sprintf("Leaked Variables: %d\n\n", len(leaked))

	for _, ptr := range leaked {
		report += fmt.Sprintf("Type: *%s\n", ptr.TypeName)
		report += fmt.Sprintf("  Pointer: sha256:%s\n", ptr.Hash)
		report += fmt.Sprintf("  Allocated: line %d, column %d\n", ptr.Metadata.AllocLine, ptr.Metadata.AllocColumn)

		if ptr.Metadata.LastWrite != nil {
			report += fmt.Sprintf("  Last Write: line %d, column %d\n", ptr.Metadata.LastWrite.Line, ptr.Metadata.LastWrite.Column)
		} else {
			report += "  Last Write: never\n"
		}

		if ptr.Metadata.LastRead != nil {
			report += fmt.Sprintf("  Last Read: line %d, column %d\n", ptr.Metadata.LastRead.Line, ptr.Metadata.LastRead.Column)
		} else {
			report += "  Last Read: never\n"
		}

		report += "  Status: NOT FREED\n\n"
	}

	report += fmt.Sprintf("Total Allocated: %d\n", totalAllocated)
	report += fmt.Sprintf("Total Freed: %d\n", totalFreed)

	if totalAllocated > 0 {
		efficiency := float64(totalFreed) / float64(totalAllocated) * 100
		report += fmt.Sprintf("Memory Efficiency: %.2f%%\n", efficiency)
	}

	return report
}
