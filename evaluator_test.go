package main

import (
	"testing"
)

func TestEvalIntegerExpression(t *testing.T) {
	tests := []struct {
		input    string
		expected int64
	}{
		{"x = 5", 5},
		{"x = 10", 10},
		{"x = -5", -5},
		{"x = 5 + 5", 10},
		{"x = 5 - 3", 2},
		{"x = 5 * 2", 10},
		{"x = 10 / 2", 5},
		{"x = 10 % 3", 1},
		{"x = 2 + 3 * 4", 14},
		{"x = (2 + 3) * 4", 20},
	}

	for _, tt := range tests {
		evaluated := testEval(tt.input)
		testIntegerObject(t, evaluated, tt.expected)
	}
}

func TestEvalBooleanExpression(t *testing.T) {
	tests := []struct {
		input    string
		expected bool
	}{
		{"x = true", true},
		{"x = false", false},
		{"x = 1 < 2", true},
		{"x = 1 > 2", false},
		{"x = 1 == 1", true},
		{"x = 1 != 2", true},
		{"x = 1 == 2", false},
		{"x = true and true", true},
		{"x = true and false", false},
		{"x = false or true", true},
		{"x = false or false", false},
	}

	for _, tt := range tests {
		evaluated := testEval(tt.input)
		testBooleanObject(t, evaluated, tt.expected)
	}
}

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
