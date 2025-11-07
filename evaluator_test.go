package main

import (
	"testing"
)

func TestNotOperator(t *testing.T) {
	tests := []struct {
		input    string
		expected bool
	}{
		{"x = not true", false},
		{"x = not false", true},
		{"x = not nil", true},
	}

	for _, tt := range tests {
		evaluated := testEval(tt.input)
		testBooleanObject(t, evaluated, tt.expected)
	}
}

func TestImmutableVariable(t *testing.T) {
	input := `
	x = 10
	x = 20
	`

	evaluated := testEval(input)
	errObj, ok := evaluated.(*Error)
	if !ok {
		t.Fatalf("expected error object, got %T (%+v)", evaluated, evaluated)
	}

	expectedMessage := "cannot reassign immutable variable 'x'"
	if errObj.Message[:len(expectedMessage)] != expectedMessage {
		t.Errorf("wrong error message. expected to start with=%q, got=%q",
			expectedMessage, errObj.Message)
	}
}

func TestMutableVariable(t *testing.T) {
	input := `
	mut x = 10
	x = 20
	`

	evaluated := testEval(input)
	testIntegerObject(t, evaluated, 20)
}

func TestMultipleVariables(t *testing.T) {
	input := `
	x = 10
	mut y = 20
	y = y + x
	`

	evaluated := testEval(input)
	testIntegerObject(t, evaluated, 30)
}

func TestVariableReference(t *testing.T) {
	input := `
	x = 10
	y = x
	`

	eval := NewEvaluator()
	lexer := NewLexer(input)
	parser := NewParser(lexer)
	program := parser.ParseProgram()
	eval.Eval(program)

	// Check that y has the value of x
	variable, ok := eval.env.Get("y")
	if !ok {
		t.Fatalf("variable y not found")
	}

	testIntegerObject(t, variable.Value, 10)
}

func TestUndefinedVariable(t *testing.T) {
	input := "y = x"

	evaluated := testEval(input)
	errObj, ok := evaluated.(*Error)
	if !ok {
		t.Fatalf("expected error object, got %T (%+v)", evaluated, evaluated)
	}

	expectedMessage := "identifier not found: x"
	if errObj.Message != expectedMessage {
		t.Errorf("wrong error message. expected=%q, got=%q",
			expectedMessage, errObj.Message)
	}
}

func TestStringConcatenation(t *testing.T) {
	input := `x = "Hello" .. " " .. "World"`

	evaluated := testEval(input)
	str, ok := evaluated.(*String)
	if !ok {
		t.Fatalf("object is not String. got=%T (%+v)", evaluated, evaluated)
	}

	if str.Value != "Hello World" {
		t.Errorf("String has wrong value. got=%q", str.Value)
	}
}

func TestStringLength(t *testing.T) {
	input := `x = #"Hello"`

	evaluated := testEval(input)
	testIntegerObject(t, evaluated, 5)
}

func TestNilValue(t *testing.T) {
	input := `x = nil`

	evaluated := testEval(input)
	if evaluated.Type() != NIL_OBJ {
		t.Errorf("object is not Nil. got=%T (%+v)", evaluated, evaluated)
	}
}

func TestMutableityTracking(t *testing.T) {
	input := `
	x = 10
	mut y = 20
	`

	eval := NewEvaluator()
	lexer := NewLexer(input)
	parser := NewParser(lexer)
	program := parser.ParseProgram()
	eval.Eval(program)

	// Check x is immutable
	xVar, ok := eval.env.Get("x")
	if !ok {
		t.Fatalf("variable x not found")
	}
	if xVar.Mutable {
		t.Errorf("x should be immutable")
	}

	// Check y is mutable
	yVar, ok := eval.env.Get("y")
	if !ok {
		t.Fatalf("variable y not found")
	}
	if !yVar.Mutable {
		t.Errorf("y should be mutable")
	}
}

func TestComplexExpression(t *testing.T) {
	tests := []struct {
		input    string
		expected int64
	}{
		{"result = 2 * (3 + 4)", 14},
		{"result = (5 + 10 * 2 + 15 / 3) * 2", 60}, // (5 + 20 + 5) * 2 = 60
		{"result = 10 + -5", 5},
	}

	for _, tt := range tests {
		evaluated := testEval(tt.input)
		testIntegerObject(t, evaluated, tt.expected)
	}
}

func TestDivisionByZero(t *testing.T) {
	input := `x = 10 / 0`

	evaluated := testEval(input)
	errObj, ok := evaluated.(*Error)
	if !ok {
		t.Fatalf("expected error object, got %T (%+v)", evaluated, evaluated)
	}

	if errObj.Message != "division by zero" {
		t.Errorf("wrong error message. expected=%q, got=%q",
			"division by zero", errObj.Message)
	}
}

func TestChainedAssignments(t *testing.T) {
	input := `
	x = 5
	y = x + 10
	mut z = y * 2
	z = z + x
	`

	eval := NewEvaluator()
	lexer := NewLexer(input)
	parser := NewParser(lexer)
	program := parser.ParseProgram()
	eval.Eval(program)

	// Check final value of z
	zVar, ok := eval.env.Get("z")
	if !ok {
		t.Fatalf("variable z not found")
	}

	testIntegerObject(t, zVar.Value, 35) // (5 + 10) * 2 + 5 = 35
}

func TestEnvironmentGetAllVariables(t *testing.T) {
	input := `
	x = 10
	mut y = 20
	name = "Alice"
	`

	eval := NewEvaluator()
	lexer := NewLexer(input)
	parser := NewParser(lexer)
	program := parser.ParseProgram()
	eval.Eval(program)

	vars := eval.env.GetAllVariables()

	if len(vars) != 3 {
		t.Fatalf("expected 3 variables, got %d", len(vars))
	}

	if _, ok := vars["x"]; !ok {
		t.Errorf("variable x not found")
	}
	if _, ok := vars["y"]; !ok {
		t.Errorf("variable y not found")
	}
	if _, ok := vars["name"]; !ok {
		t.Errorf("variable name not found")
	}
}

// Helper functions

func testEval(input string) Object {
	lexer := NewLexer(input)
	parser := NewParser(lexer)
	program := parser.ParseProgram()
	eval := NewEvaluator()

	return eval.Eval(program)
}

func testIntegerObject(t *testing.T, obj Object, expected int64) bool {
	result, ok := obj.(*Integer)
	if !ok {
		t.Errorf("object is not Integer. got=%T (%+v)", obj, obj)
		return false
	}

	if result.Value != expected {
		t.Errorf("object has wrong value. got=%d, want=%d",
			result.Value, expected)
		return false
	}

	return true
}

func testBooleanObject(t *testing.T, obj Object, expected bool) bool {
	result, ok := obj.(*Boolean)
	if !ok {
		t.Errorf("object is not Boolean. got=%T (%+v)", obj, obj)
		return false
	}

	if result.Value != expected {
		t.Errorf("object has wrong value. got=%t, want=%t",
			result.Value, expected)
		return false
	}

	return true
}

func TestMakeFunction(t *testing.T) {
	input := `ptr = make(int, 100)`

	eval := NewEvaluator()
	lexer := NewLexer(input)
	parser := NewParser(lexer)
	program := parser.ParseProgram()
	eval.Eval(program)

	// Check that ptr is a pointer
	ptrVar, ok := eval.env.Get("ptr")
	if !ok {
		t.Fatalf("variable ptr not found")
	}

	pointer, ok := ptrVar.Value.(*Pointer)
	if !ok {
		t.Fatalf("ptr is not a Pointer. got=%T", ptrVar.Value)
	}

	if pointer.TypeName != "int" {
		t.Errorf("pointer type wrong. got=%s, want=int", pointer.TypeName)
	}

	if pointer.Size != 100 {
		t.Errorf("pointer size wrong. got=%d, want=100", pointer.Size)
	}

	if pointer.Freed {
		t.Errorf("pointer should not be freed")
	}

	if len(pointer.Hash) != 64 { // SHA256 produces 64 hex chars
		t.Errorf("pointer hash wrong length. got=%d, want=64", len(pointer.Hash))
	}
}

func TestMakeAndFree(t *testing.T) {
	input := `
	ptr = make(int, 100)
	free(ptr)
	`

	eval := NewEvaluator()
	lexer := NewLexer(input)
	parser := NewParser(lexer)
	program := parser.ParseProgram()
	result := eval.Eval(program)

	if isError(result) {
		t.Fatalf("evaluation error: %s", result.Inspect())
	}

	// Check in memory tracker that the pointer is freed
	var found bool
	var freed bool
	for _, ptr := range eval.memory.allocations {
		found = true
		freed = ptr.Freed
		break // Only one allocation
	}

	if !found {
		t.Fatalf("no allocations found in memory tracker")
	}

	if !freed {
		t.Errorf("pointer should be freed in memory tracker")
	}
}

func TestDoubleFree(t *testing.T) {
	input := `
	ptr = make(int, 100)
	free(ptr)
	free(ptr)
	`

	eval := NewEvaluator()
	lexer := NewLexer(input)
	parser := NewParser(lexer)
	program := parser.ParseProgram()
	result := eval.Eval(program)

	errObj, ok := result.(*Error)
	if !ok {
		t.Fatalf("expected error object, got %T (%+v)", result, result)
	}

	expectedMessage := "double free detected"
	if len(errObj.Message) < len(expectedMessage) || errObj.Message[:len(expectedMessage)] != expectedMessage {
		t.Errorf("wrong error message. expected to start with=%q, got=%q",
			expectedMessage, errObj.Message)
	}
}

func TestFreeNonPointer(t *testing.T) {
	input := `
	x = 10
	free(x)
	`

	eval := NewEvaluator()
	lexer := NewLexer(input)
	parser := NewParser(lexer)
	program := parser.ParseProgram()
	result := eval.Eval(program)

	errObj, ok := result.(*Error)
	if !ok {
		t.Fatalf("expected error object, got %T (%+v)", result, result)
	}

	expectedMessage := "free() requires a pointer"
	if len(errObj.Message) < len(expectedMessage) || errObj.Message[:len(expectedMessage)] != expectedMessage {
		t.Errorf("wrong error message. expected to start with=%q, got=%q",
			expectedMessage, errObj.Message)
	}
}

func TestMakeInvalidArguments(t *testing.T) {
	tests := []struct {
		input           string
		expectedMessage string
	}{
		{`ptr = make(int)`, "make() requires 2 arguments"},
		{`ptr = make(int, "abc")`, "make() second argument must be an integer"},
		{`ptr = make(int, 0)`, "make() size must be positive"},
		{`ptr = make(int, -5)`, "make() size must be positive"},
	}

	for _, tt := range tests {
		evaluated := testEval(tt.input)
		errObj, ok := evaluated.(*Error)
		if !ok {
			t.Fatalf("expected error object for input %q, got %T (%+v)", tt.input, evaluated, evaluated)
		}

		if len(errObj.Message) < len(tt.expectedMessage) || errObj.Message[:len(tt.expectedMessage)] != tt.expectedMessage {
			t.Errorf("wrong error message for input %q. expected to start with=%q, got=%q",
				tt.input, tt.expectedMessage, errObj.Message)
		}
	}
}

func TestMultipleAllocations(t *testing.T) {
	input := `
	ptr1 = make(int, 10)
	ptr2 = make(string, 20)
	ptr3 = make(float, 30)
	`

	eval := NewEvaluator()
	lexer := NewLexer(input)
	parser := NewParser(lexer)
	program := parser.ParseProgram()
	eval.Eval(program)

	// Check all three allocations
	if len(eval.memory.allocations) != 3 {
		t.Fatalf("expected 3 allocations, got %d", len(eval.memory.allocations))
	}
}

func TestMemoryLeakDetection(t *testing.T) {
	input := `
	ptr1 = make(int, 10)
	ptr2 = make(string, 20)
	free(ptr1)
	`

	eval := NewEvaluator()
	lexer := NewLexer(input)
	parser := NewParser(lexer)
	program := parser.ParseProgram()
	eval.Eval(program)

	// Generate free report
	report := eval.GetFreeReport()

	// Check that report mentions 1 leaked variable
	if len(report) == 0 {
		t.Fatalf("expected free report, got empty string")
	}

	// Should have 1 leaked (ptr2)
	leaked := 0
	for _, ptr := range eval.memory.allocations {
		if !ptr.Freed {
			leaked++
		}
	}

	if leaked != 1 {
		t.Errorf("expected 1 leaked pointer, got %d", leaked)
	}
}

func TestPointerInspect(t *testing.T) {
	input := `ptr = make(int, 100)`

	eval := NewEvaluator()
	lexer := NewLexer(input)
	parser := NewParser(lexer)
	program := parser.ParseProgram()
	eval.Eval(program)

	ptrVar, _ := eval.env.Get("ptr")
	pointer := ptrVar.Value.(*Pointer)

	inspect := pointer.Inspect()
	if len(inspect) < 10 {
		t.Errorf("pointer inspect string too short: %s", inspect)
	}

	// Should contain "sha256:"
	if inspect[:7] != "sha256:" {
		t.Errorf("pointer inspect should start with 'sha256:', got: %s", inspect)
	}
}

func TestPointerUniqueHashes(t *testing.T) {
	input := `
	ptr1 = make(int, 100)
	ptr2 = make(int, 100)
	`

	eval := NewEvaluator()
	lexer := NewLexer(input)
	parser := NewParser(lexer)
	program := parser.ParseProgram()
	eval.Eval(program)

	ptr1Var, _ := eval.env.Get("ptr1")
	ptr2Var, _ := eval.env.Get("ptr2")

	ptr1 := ptr1Var.Value.(*Pointer)
	ptr2 := ptr2Var.Value.(*Pointer)

	if ptr1.Hash == ptr2.Hash {
		t.Errorf("pointers should have unique hashes, both got: %s", ptr1.Hash)
	}
}

func TestIfStatement(t *testing.T) {
	tests := []struct {
		input    string
		expected int64
	}{
		{
			`
			mut x = 0
			if true then
				x = 10
			end
			`,
			10,
		},
		{
			`
			mut x = 0
			if false then
				x = 10
			else
				x = 20
			end
			`,
			20,
		},
		{
			`
			mut x = 0
			if false then
				x = 10
			elseif true then
				x = 30
			else
				x = 20
			end
			`,
			30,
		},
		{
			`
			mut x = 0
			if 5 > 10 then
				x = 10
			else
				x = 20
			end
			`,
			20,
		},
		{
			`
			mut x = 0
			if 10 > 5 then
				x = 10
			end
			`,
			10,
		},
	}

	for i, tt := range tests {
		eval := NewEvaluator()
		lexer := NewLexer(tt.input)
		parser := NewParser(lexer)
		program := parser.ParseProgram()

		// Check for parser errors
		if len(parser.Errors()) > 0 {
			t.Errorf("test %d: parser errors:", i)
			for _, e := range parser.Errors() {
				t.Errorf("  %s", e)
			}
			continue
		}

		result := eval.Eval(program)

		// Check for evaluation errors
		if result != nil && result.Type() == ERROR_OBJ {
			t.Errorf("test %d: evaluation error: %s", i, result.Inspect())
			continue
		}

		xVar, ok := eval.env.Get("x")
		if !ok {
			t.Errorf("test %d: variable x not found", i)
			continue
		}

		intVal, ok := xVar.Value.(*Integer)
		if !ok {
			t.Errorf("test %d: expected Integer, got %T", i, xVar.Value)
			continue
		}

		if intVal.Value != tt.expected {
			t.Errorf("test %d: expected %d, got %d", i, tt.expected, intVal.Value)
		}
	}
}

func TestNestedIfStatement(t *testing.T) {
	input := `
	mut x = 0
	if true then
		if true then
			x = 42
		end
	end
	`

	eval := NewEvaluator()
	lexer := NewLexer(input)
	parser := NewParser(lexer)
	program := parser.ParseProgram()
	eval.Eval(program)

	xVar, _ := eval.env.Get("x")
	if xVar == nil {
		t.Fatal("variable x not found")
	}

	intVal, ok := xVar.Value.(*Integer)
	if !ok {
		t.Fatalf("expected Integer, got %T", xVar.Value)
	}

	if intVal.Value != 42 {
		t.Errorf("expected 42, got %d", intVal.Value)
	}
}

func TestIfWithMutableVariable(t *testing.T) {
	input := `
	mut counter = 0
	if true then
		counter = 10
	end
	if counter == 10 then
		counter = 20
	end
	`

	eval := NewEvaluator()
	lexer := NewLexer(input)
	parser := NewParser(lexer)
	program := parser.ParseProgram()
	eval.Eval(program)

	counterVar, _ := eval.env.Get("counter")
	if counterVar == nil {
		t.Fatal("variable counter not found")
	}

	intVal, ok := counterVar.Value.(*Integer)
	if !ok {
		t.Fatalf("expected Integer, got %T", counterVar.Value)
	}

	if intVal.Value != 20 {
		t.Errorf("expected 20, got %d", intVal.Value)
	}
}

func TestNumericForLoop(t *testing.T) {
	input := `
	mut sum = 0
	for i = 1, 5 do
		sum = sum + i
	end
	`

	eval := NewEvaluator()
	lexer := NewLexer(input)
	parser := NewParser(lexer)
	program := parser.ParseProgram()
	eval.Eval(program)

	sumVar, _ := eval.env.Get("sum")
	if sumVar == nil {
		t.Fatal("variable sum not found")
	}

	intVal, ok := sumVar.Value.(*Integer)
	if !ok {
		t.Fatalf("expected Integer, got %T", sumVar.Value)
	}

	// sum of 1+2+3+4+5 = 15
	if intVal.Value != 15 {
		t.Errorf("expected 15, got %d", intVal.Value)
	}
}

func TestNumericForLoopWithStep(t *testing.T) {
	input := `
	mut sum = 0
	for i = 0, 10, 2 do
		sum = sum + i
	end
	`

	eval := NewEvaluator()
	lexer := NewLexer(input)
	parser := NewParser(lexer)
	program := parser.ParseProgram()
	eval.Eval(program)

	sumVar, _ := eval.env.Get("sum")
	if sumVar == nil {
		t.Fatal("variable sum not found")
	}

	intVal, ok := sumVar.Value.(*Integer)
	if !ok {
		t.Fatalf("expected Integer, got %T", sumVar.Value)
	}

	// sum of 0+2+4+6+8+10 = 30
	if intVal.Value != 30 {
		t.Errorf("expected 30, got %d", intVal.Value)
	}
}

func TestNumericForLoopNegativeStep(t *testing.T) {
	input := `
	mut sum = 0
	for i = 5, 1, -1 do
		sum = sum + i
	end
	`

	eval := NewEvaluator()
	lexer := NewLexer(input)
	parser := NewParser(lexer)
	program := parser.ParseProgram()
	eval.Eval(program)

	sumVar, _ := eval.env.Get("sum")
	if sumVar == nil {
		t.Fatal("variable sum not found")
	}

	intVal, ok := sumVar.Value.(*Integer)
	if !ok {
		t.Fatalf("expected Integer, got %T", sumVar.Value)
	}

	// sum of 5+4+3+2+1 = 15
	if intVal.Value != 15 {
		t.Errorf("expected 15, got %d", intVal.Value)
	}
}

func TestForLoopScope(t *testing.T) {
	input := `
	mut x = 100
	for i = 1, 3 do
		x = x + i
	end
	`

	eval := NewEvaluator()
	lexer := NewLexer(input)
	parser := NewParser(lexer)
	program := parser.ParseProgram()
	eval.Eval(program)

	// Check that i doesn't leak outside loop
	_, exists := eval.env.Get("i")
	if exists {
		t.Error("loop variable 'i' should not exist outside loop scope")
	}

	// Check x was modified
	xVar, _ := eval.env.Get("x")
	if xVar == nil {
		t.Fatal("variable x not found")
	}

	intVal, ok := xVar.Value.(*Integer)
	if !ok {
		t.Fatalf("expected Integer, got %T", xVar.Value)
	}

	// 100 + 1 + 2 + 3 = 106
	if intVal.Value != 106 {
		t.Errorf("expected 106, got %d", intVal.Value)
	}
}

func TestNestedForLoop(t *testing.T) {
	input := `
	mut sum = 0
	for i = 1, 3 do
		for j = 1, 2 do
			sum = sum + i * j
		end
	end
	`

	eval := NewEvaluator()
	lexer := NewLexer(input)
	parser := NewParser(lexer)
	program := parser.ParseProgram()
	eval.Eval(program)

	sumVar, _ := eval.env.Get("sum")
	if sumVar == nil {
		t.Fatal("variable sum not found")
	}

	intVal, ok := sumVar.Value.(*Integer)
	if !ok {
		t.Fatalf("expected Integer, got %T", sumVar.Value)
	}

	// (1*1 + 1*2) + (2*1 + 2*2) + (3*1 + 3*2) = 3 + 6 + 9 = 18
	if intVal.Value != 18 {
		t.Errorf("expected 18, got %d", intVal.Value)
	}
}

// Test immutability: attempt to modify an immutable variable should fail
func TestImmutableVariableCannotBeModified(t *testing.T) {
	input := `
	x = 10
	x = 20
	`

	eval := NewEvaluator()
	lexer := NewLexer(input)
	parser := NewParser(lexer)
	program := parser.ParseProgram()
	result := eval.Eval(program)

	// Should return an error
	if result == nil {
		t.Fatal("Expected error when modifying immutable variable")
	}

	errObj, ok := result.(*Error)
	if !ok {
		t.Fatalf("Expected Error object, got %T", result)
	}

	if errObj.Message == "" {
		t.Error("Expected error message about immutable variable")
	}
}

// Test mutable variable can be modified
func TestMutableVariableCanBeModified(t *testing.T) {
	input := `
	mut x = 10
	x = 20
	`

	eval := NewEvaluator()
	lexer := NewLexer(input)
	parser := NewParser(lexer)
	program := parser.ParseProgram()
	eval.Eval(program)

	xVar, _ := eval.env.Get("x")
	if xVar == nil {
		t.Fatal("variable x not found")
	}

	intVal, ok := xVar.Value.(*Integer)
	if !ok {
		t.Fatalf("expected Integer, got %T", xVar.Value)
	}

	if intVal.Value != 20 {
		t.Errorf("expected 20, got %d", intVal.Value)
	}
}

// Test loop variable is immutable within loop
func TestLoopVariableIsImmutable(t *testing.T) {
	input := `
	for i = 1, 3 do
		i = 10
	end
	`

	eval := NewEvaluator()
	lexer := NewLexer(input)
	parser := NewParser(lexer)
	program := parser.ParseProgram()
	result := eval.Eval(program)

	// Should return an error
	if result == nil {
		t.Fatal("Expected error when modifying loop variable")
	}

	errObj, ok := result.(*Error)
	if !ok {
		t.Fatalf("Expected Error object, got %T", result)
	}

	if errObj.Message == "" {
		t.Error("Expected error message about immutable loop variable")
	}
}

// Test immutability in nested scopes
func TestImmutabilityInNestedScopes(t *testing.T) {
	input := `
	x = 5
	for i = 1, 2 do
		x = 10
	end
	`

	eval := NewEvaluator()
	lexer := NewLexer(input)
	parser := NewParser(lexer)
	program := parser.ParseProgram()
	result := eval.Eval(program)

	// Should return an error
	if result == nil {
		t.Fatal("Expected error when modifying immutable variable in nested scope")
	}

	errObj, ok := result.(*Error)
	if !ok {
		t.Fatalf("Expected Error object, got %T", result)
	}

	if errObj.Message == "" {
		t.Error("Expected error message about immutable variable in nested scope")
	}
}

// Test mutable variable modified in loop
func TestMutableVariableInLoop(t *testing.T) {
	input := `
	mut counter = 0
	for i = 1, 5 do
		counter = counter + 1
	end
	`

	eval := NewEvaluator()
	lexer := NewLexer(input)
	parser := NewParser(lexer)
	program := parser.ParseProgram()
	eval.Eval(program)

	counterVar, _ := eval.env.Get("counter")
	if counterVar == nil {
		t.Fatal("variable counter not found")
	}

	intVal, ok := counterVar.Value.(*Integer)
	if !ok {
		t.Fatalf("expected Integer, got %T", counterVar.Value)
	}

	if intVal.Value != 5 {
		t.Errorf("expected 5, got %d", intVal.Value)
	}
}

// Test variable update in nested scope (no shadowing - updates outer variable)
func TestVariableUpdateInNestedScope(t *testing.T) {
	input := `
	mut x = 100
	mut sum = 0
	for i = 1, 3 do
		x = i * 10
		sum = sum + x
	end
	`

	eval := NewEvaluator()
	lexer := NewLexer(input)
	parser := NewParser(lexer)
	program := parser.ParseProgram()
	eval.Eval(program)

	// x should be modified to 30 (last iteration)
	xVar, _ := eval.env.Get("x")
	if xVar == nil {
		t.Fatal("variable x not found")
	}

	intVal, ok := xVar.Value.(*Integer)
	if !ok {
		t.Fatalf("expected Integer, got %T", xVar.Value)
	}

	if intVal.Value != 30 {
		t.Errorf("x should be 30 (last loop value), got %d", intVal.Value)
	}

	// Sum should be 10 + 20 + 30 = 60
	sumVar, _ := eval.env.Get("sum")
	if sumVar == nil {
		t.Fatal("variable sum not found")
	}

	sumIntVal, ok := sumVar.Value.(*Integer)
	if !ok {
		t.Fatalf("expected Integer, got %T", sumVar.Value)
	}

	if sumIntVal.Value != 60 {
		t.Errorf("expected sum to be 60, got %d", sumIntVal.Value)
	}
}

// Test multiple mutable variables in loop
func TestMultipleMutableVariablesInLoop(t *testing.T) {
	input := `
	mut a = 0
	mut b = 1
	for i = 1, 3 do
		mut temp = a + b
		a = b
		b = temp
	end
	`

	eval := NewEvaluator()
	lexer := NewLexer(input)
	parser := NewParser(lexer)
	program := parser.ParseProgram()
	eval.Eval(program)

	// After 3 iterations: (0,1) -> (1,1) -> (1,2) -> (2,3)
	aVar, _ := eval.env.Get("a")
	if aVar == nil {
		t.Fatal("variable a not found")
	}

	aIntVal, ok := aVar.Value.(*Integer)
	if !ok {
		t.Fatalf("expected Integer, got %T", aVar.Value)
	}

	if aIntVal.Value != 2 {
		t.Errorf("expected a to be 2, got %d", aIntVal.Value)
	}

	bVar, _ := eval.env.Get("b")
	if bVar == nil {
		t.Fatal("variable b not found")
	}

	bIntVal, ok := bVar.Value.(*Integer)
	if !ok {
		t.Fatalf("expected Integer, got %T", bVar.Value)
	}

	if bIntVal.Value != 3 {
		t.Errorf("expected b to be 3, got %d", bIntVal.Value)
	}

	// temp should not exist outside loop
	tempVar, exists := eval.env.Get("temp")
	if exists && tempVar != nil {
		t.Error("loop variable 'temp' should not exist outside loop scope")
	}
}

// Test immutable variable access in nested loops
func TestImmutableVariableAccessInNestedLoops(t *testing.T) {
	input := `
	multiplier = 2
	mut sum = 0
	for i = 1, 3 do
		for j = 1, 2 do
			sum = sum + (i * j * multiplier)
		end
	end
	`

	eval := NewEvaluator()
	lexer := NewLexer(input)
	parser := NewParser(lexer)
	program := parser.ParseProgram()
	eval.Eval(program)

	sumVar, _ := eval.env.Get("sum")
	if sumVar == nil {
		t.Fatal("variable sum not found")
	}

	intVal, ok := sumVar.Value.(*Integer)
	if !ok {
		t.Fatalf("expected Integer, got %T", sumVar.Value)
	}

	// (1*1*2 + 1*2*2) + (2*1*2 + 2*2*2) + (3*1*2 + 3*2*2) = 6 + 12 + 18 = 36
	if intVal.Value != 36 {
		t.Errorf("expected 36, got %d", intVal.Value)
	}
}

// Test loop with only reads (no mutations)
func TestLoopWithOnlyReads(t *testing.T) {
	input := `
	x = 10
	y = 20
	mut sum = 0
	for i = 1, 3 do
		sum = sum + x + y + i
	end
	`

	eval := NewEvaluator()
	lexer := NewLexer(input)
	parser := NewParser(lexer)
	program := parser.ParseProgram()
	eval.Eval(program)

	sumVar, _ := eval.env.Get("sum")
	if sumVar == nil {
		t.Fatal("variable sum not found")
	}

	intVal, ok := sumVar.Value.(*Integer)
	if !ok {
		t.Fatalf("expected Integer, got %T", sumVar.Value)
	}

	// (10+20+1) + (10+20+2) + (10+20+3) = 31 + 32 + 33 = 96
	if intVal.Value != 96 {
		t.Errorf("expected 96, got %d", intVal.Value)
	}
}

// Test empty loop body preserves immutability
func TestEmptyLoopBody(t *testing.T) {
	input := `
	x = 5
	for i = 1, 3 do
	end
	`

	eval := NewEvaluator()
	lexer := NewLexer(input)
	parser := NewParser(lexer)
	program := parser.ParseProgram()
	eval.Eval(program)

	xVar, _ := eval.env.Get("x")
	if xVar == nil {
		t.Fatal("variable x not found")
	}

	intVal, ok := xVar.Value.(*Integer)
	if !ok {
		t.Fatalf("expected Integer, got %T", xVar.Value)
	}

	if intVal.Value != 5 {
		t.Errorf("expected 5, got %d", intVal.Value)
	}

	// Loop variable should not exist
	iVar, exists := eval.env.Get("i")
	if exists && iVar != nil {
		t.Error("loop variable 'i' should not exist outside loop scope")
	}
}
