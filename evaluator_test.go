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
