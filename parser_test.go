package main

import (
	"testing"
)

func TestImmutableAssignment(t *testing.T) {
	input := `x = 10`

	l := NewLexer(input)
	p := NewParser(l)
	program := p.ParseProgram()
	checkParserErrors(t, p)

	if len(program.Statements) != 1 {
		t.Fatalf("program.Statements does not contain 1 statement. got=%d",
			len(program.Statements))
	}

	stmt, ok := program.Statements[0].(*AssignmentStatement)
	if !ok {
		t.Fatalf("program.Statements[0] is not *AssignmentStatement. got=%T",
			program.Statements[0])
	}

	if stmt.Mutable {
		t.Errorf("stmt.Mutable is true, expected false")
	}

	if stmt.Name.Value != "x" {
		t.Errorf("stmt.Name.Value not 'x'. got=%s", stmt.Name.Value)
	}

	testLiteralExpression(t, stmt.Value, "10")
}

func TestMutableAssignment(t *testing.T) {
	input := `mut y = 20`

	l := NewLexer(input)
	p := NewParser(l)
	program := p.ParseProgram()
	checkParserErrors(t, p)

	if len(program.Statements) != 1 {
		t.Fatalf("program.Statements does not contain 1 statement. got=%d",
			len(program.Statements))
	}

	stmt, ok := program.Statements[0].(*AssignmentStatement)
	if !ok {
		t.Fatalf("program.Statements[0] is not *AssignmentStatement. got=%T",
			program.Statements[0])
	}

	if !stmt.Mutable {
		t.Errorf("stmt.Mutable is false, expected true")
	}

	if stmt.Name.Value != "y" {
		t.Errorf("stmt.Name.Value not 'y'. got=%s", stmt.Name.Value)
	}

	testLiteralExpression(t, stmt.Value, "20")
}

func TestMultipleAssignments(t *testing.T) {
	input := `
	x = 10
	mut y = 20
	name = "Alice"
	`

	l := NewLexer(input)
	p := NewParser(l)
	program := p.ParseProgram()
	checkParserErrors(t, p)

	if len(program.Statements) != 3 {
		t.Fatalf("program.Statements does not contain 3 statements. got=%d",
			len(program.Statements))
	}

	tests := []struct {
		expectedName    string
		expectedMutable bool
	}{
		{"x", false},
		{"y", true},
		{"name", false},
	}

	for i, tt := range tests {
		stmt := program.Statements[i]
		if !testAssignmentStatement(t, stmt, tt.expectedName, tt.expectedMutable) {
			return
		}
	}
}

func TestAssignmentWithExpression(t *testing.T) {
	tests := []struct {
		input    string
		name     string
		mutable  bool
		expected string
	}{
		{"x = 10 + 5", "x", false, "(10 + 5)"},
		{"mut y = 20 * 2", "y", true, "(20 * 2)"},
		{"z = 10 + 5 * 2", "z", false, "(10 + (5 * 2))"},
		{"a = (10 + 5) * 2", "a", false, "((10 + 5) * 2)"},
	}

	for _, tt := range tests {
		l := NewLexer(tt.input)
		p := NewParser(l)
		program := p.ParseProgram()
		checkParserErrors(t, p)

		if len(program.Statements) != 1 {
			t.Fatalf("program.Statements does not contain 1 statement. got=%d",
				len(program.Statements))
		}

		stmt, ok := program.Statements[0].(*AssignmentStatement)
		if !ok {
			t.Fatalf("program.Statements[0] is not *AssignmentStatement. got=%T",
				program.Statements[0])
		}

		if stmt.Name.Value != tt.name {
			t.Errorf("stmt.Name.Value not '%s'. got=%s", tt.name, stmt.Name.Value)
		}

		if stmt.Mutable != tt.mutable {
			t.Errorf("stmt.Mutable not %v. got=%v", tt.mutable, stmt.Mutable)
		}

		if stmt.Value.String() != tt.expected {
			t.Errorf("stmt.Value.String() not '%s'. got=%s", tt.expected, stmt.Value.String())
		}
	}
}

func TestAssignmentWithString(t *testing.T) {
	input := `name = "Alice"`

	l := NewLexer(input)
	p := NewParser(l)
	program := p.ParseProgram()
	checkParserErrors(t, p)

	if len(program.Statements) != 1 {
		t.Fatalf("program.Statements does not contain 1 statement. got=%d",
			len(program.Statements))
	}

	stmt, ok := program.Statements[0].(*AssignmentStatement)
	if !ok {
		t.Fatalf("program.Statements[0] is not *AssignmentStatement. got=%T",
			program.Statements[0])
	}

	strLit, ok := stmt.Value.(*StringLiteral)
	if !ok {
		t.Fatalf("stmt.Value is not *StringLiteral. got=%T", stmt.Value)
	}

	if strLit.Value != "Alice" {
		t.Errorf("strLit.Value not 'Alice'. got=%s", strLit.Value)
	}
}

func TestAssignmentWithBoolean(t *testing.T) {
	tests := []struct {
		input    string
		name     string
		expected bool
	}{
		{"flag = true", "flag", true},
		{"mut done = false", "done", false},
	}

	for _, tt := range tests {
		l := NewLexer(tt.input)
		p := NewParser(l)
		program := p.ParseProgram()
		checkParserErrors(t, p)

		if len(program.Statements) != 1 {
			t.Fatalf("program.Statements does not contain 1 statement. got=%d",
				len(program.Statements))
		}

		stmt, ok := program.Statements[0].(*AssignmentStatement)
		if !ok {
			t.Fatalf("program.Statements[0] is not *AssignmentStatement. got=%T",
				program.Statements[0])
		}

		boolLit, ok := stmt.Value.(*BooleanLiteral)
		if !ok {
			t.Fatalf("stmt.Value is not *BooleanLiteral. got=%T", stmt.Value)
		}

		if boolLit.Value != tt.expected {
			t.Errorf("boolLit.Value not %v. got=%v", tt.expected, boolLit.Value)
		}
	}
}

func TestAssignmentWithNil(t *testing.T) {
	input := `ptr = nil`

	l := NewLexer(input)
	p := NewParser(l)
	program := p.ParseProgram()
	checkParserErrors(t, p)

	if len(program.Statements) != 1 {
		t.Fatalf("program.Statements does not contain 1 statement. got=%d",
			len(program.Statements))
	}

	stmt, ok := program.Statements[0].(*AssignmentStatement)
	if !ok {
		t.Fatalf("program.Statements[0] is not *AssignmentStatement. got=%T",
			program.Statements[0])
	}

	_, ok = stmt.Value.(*NilLiteral)
	if !ok {
		t.Fatalf("stmt.Value is not *NilLiteral. got=%T", stmt.Value)
	}
}

func TestAssignmentWithIdentifier(t *testing.T) {
	input := `y = x`

	l := NewLexer(input)
	p := NewParser(l)
	program := p.ParseProgram()
	checkParserErrors(t, p)

	if len(program.Statements) != 1 {
		t.Fatalf("program.Statements does not contain 1 statement. got=%d",
			len(program.Statements))
	}

	stmt, ok := program.Statements[0].(*AssignmentStatement)
	if !ok {
		t.Fatalf("program.Statements[0] is not *AssignmentStatement. got=%T",
			program.Statements[0])
	}

	testIdentifier(t, stmt.Value, "x")
}

func TestAssignmentWithFunctionCall(t *testing.T) {
	input := `result = make(int, 100)`

	l := NewLexer(input)
	p := NewParser(l)
	program := p.ParseProgram()
	checkParserErrors(t, p)

	if len(program.Statements) != 1 {
		t.Fatalf("program.Statements does not contain 1 statement. got=%d",
			len(program.Statements))
	}

	stmt, ok := program.Statements[0].(*AssignmentStatement)
	if !ok {
		t.Fatalf("program.Statements[0] is not *AssignmentStatement. got=%T",
			program.Statements[0])
	}

	callExp, ok := stmt.Value.(*CallExpression)
	if !ok {
		t.Fatalf("stmt.Value is not *CallExpression. got=%T", stmt.Value)
	}

	testIdentifier(t, callExp.Function, "make")

	if len(callExp.Arguments) != 2 {
		t.Fatalf("wrong number of arguments. got=%d", len(callExp.Arguments))
	}

	testIdentifier(t, callExp.Arguments[0], "int")
	testLiteralExpression(t, callExp.Arguments[1], "100")
}

func TestAssignmentWithPrefixExpression(t *testing.T) {
	tests := []struct {
		input    string
		name     string
		operator string
		value    string
	}{
		{"x = -5", "x", "-", "5"},
		{"y = *ptr", "y", "*", "ptr"},
		{"z = &value", "z", "&", "value"},
		{"a = not flag", "a", "not", "flag"},
	}

	for _, tt := range tests {
		l := NewLexer(tt.input)
		p := NewParser(l)
		program := p.ParseProgram()
		checkParserErrors(t, p)

		if len(program.Statements) != 1 {
			t.Fatalf("program.Statements does not contain 1 statement. got=%d",
				len(program.Statements))
		}

		stmt, ok := program.Statements[0].(*AssignmentStatement)
		if !ok {
			t.Fatalf("program.Statements[0] is not *AssignmentStatement. got=%T",
				program.Statements[0])
		}

		prefixExp, ok := stmt.Value.(*PrefixExpression)
		if !ok {
			t.Fatalf("stmt.Value is not *PrefixExpression. got=%T", stmt.Value)
		}

		if prefixExp.Operator != tt.operator {
			t.Errorf("prefixExp.Operator not '%s'. got=%s", tt.operator, prefixExp.Operator)
		}
	}
}

// Helper functions

func checkParserErrors(t *testing.T, p *Parser) {
	errors := p.Errors()
	if len(errors) == 0 {
		return
	}

	t.Errorf("parser has %d errors", len(errors))
	for _, msg := range errors {
		t.Errorf("parser error: %q", msg)
	}
	t.FailNow()
}

func testAssignmentStatement(t *testing.T, s Statement, name string, mutable bool) bool {
	if s.TokenLiteral() != name && s.TokenLiteral() != "mut" {
		t.Errorf("s.TokenLiteral not '%s' or 'mut'. got=%q", name, s.TokenLiteral())
		return false
	}

	stmt, ok := s.(*AssignmentStatement)
	if !ok {
		t.Errorf("s not *AssignmentStatement. got=%T", s)
		return false
	}

	if stmt.Name.Value != name {
		t.Errorf("stmt.Name.Value not '%s'. got=%s", name, stmt.Name.Value)
		return false
	}

	if stmt.Mutable != mutable {
		t.Errorf("stmt.Mutable not %v. got=%v", mutable, stmt.Mutable)
		return false
	}

	return true
}

func testLiteralExpression(t *testing.T, exp Expression, expected string) bool {
	switch v := exp.(type) {
	case *IntegerLiteral:
		return testIntegerLiteral(t, exp, expected)
	case *Identifier:
		return testIdentifier(t, exp, expected)
	case *StringLiteral:
		if v.Value != expected {
			t.Errorf("StringLiteral.Value not %s. got=%s", expected, v.Value)
			return false
		}
		return true
	default:
		t.Errorf("type of exp not handled. got=%T", exp)
		return false
	}
}

func testIntegerLiteral(t *testing.T, il Expression, value string) bool {
	integ, ok := il.(*IntegerLiteral)
	if !ok {
		t.Errorf("il not *IntegerLiteral. got=%T", il)
		return false
	}

	if integ.Value != value {
		t.Errorf("integ.Value not %s. got=%s", value, integ.Value)
		return false
	}

	if integ.TokenLiteral() != value {
		t.Errorf("integ.TokenLiteral not %s. got=%s", value, integ.TokenLiteral())
		return false
	}

	return true
}

func testIdentifier(t *testing.T, exp Expression, value string) bool {
	ident, ok := exp.(*Identifier)
	if !ok {
		t.Errorf("exp not *Identifier. got=%T", exp)
		return false
	}

	if ident.Value != value {
		t.Errorf("ident.Value not %s. got=%s", value, ident.Value)
		return false
	}

	if ident.TokenLiteral() != value {
		t.Errorf("ident.TokenLiteral not %s. got=%s", value, ident.TokenLiteral())
		return false
	}

	return true
}

func TestParseIfStatement(t *testing.T) {
	input := `if x > 5 then y = 10 end`

	l := NewLexer(input)
	p := NewParser(l)
	program := p.ParseProgram()
	checkParserErrors(t, p)

	if len(program.Statements) != 1 {
		t.Fatalf("program.Statements does not contain 1 statement. got=%d",
			len(program.Statements))
	}

	stmt, ok := program.Statements[0].(*IfStatement)
	if !ok {
		t.Fatalf("program.Statements[0] is not *IfStatement. got=%T",
			program.Statements[0])
	}

	if stmt.Condition == nil {
		t.Fatal("stmt.Condition is nil")
	}

	if stmt.Consequence == nil {
		t.Fatal("stmt.Consequence is nil")
	}

	if len(stmt.Consequence.Statements) != 1 {
		t.Errorf("consequence is not 1 statement. got=%d",
			len(stmt.Consequence.Statements))
	}

	if stmt.Alternative != nil {
		t.Errorf("stmt.Alternative was not nil. got=%+v", stmt.Alternative)
	}
}

func TestParseIfElseStatement(t *testing.T) {
	input := `if x > 5 then y = 10 else y = 20 end`

	l := NewLexer(input)
	p := NewParser(l)
	program := p.ParseProgram()
	checkParserErrors(t, p)

	if len(program.Statements) != 1 {
		t.Fatalf("program.Statements does not contain 1 statement. got=%d",
			len(program.Statements))
	}

	stmt, ok := program.Statements[0].(*IfStatement)
	if !ok {
		t.Fatalf("program.Statements[0] is not *IfStatement. got=%T",
			program.Statements[0])
	}

	if stmt.Alternative == nil {
		t.Fatal("stmt.Alternative is nil")
	}

	if len(stmt.Alternative.Statements) != 1 {
		t.Errorf("alternative is not 1 statement. got=%d",
			len(stmt.Alternative.Statements))
	}
}

func TestParseIfElseifElseStatement(t *testing.T) {
	input := `if x > 10 then y = 1 elseif x > 5 then y = 2 else y = 3 end`

	l := NewLexer(input)
	p := NewParser(l)
	program := p.ParseProgram()
	checkParserErrors(t, p)

	if len(program.Statements) != 1 {
		t.Fatalf("program.Statements does not contain 1 statement. got=%d",
			len(program.Statements))
	}

	stmt, ok := program.Statements[0].(*IfStatement)
	if !ok {
		t.Fatalf("program.Statements[0] is not *IfStatement. got=%T",
			program.Statements[0])
	}

	if len(stmt.Alternatives) != 1 {
		t.Fatalf("stmt.Alternatives does not contain 1 elseif. got=%d",
			len(stmt.Alternatives))
	}

	if stmt.Alternative == nil {
		t.Fatal("stmt.Alternative is nil")
	}
}

func TestParseIfExpression(t *testing.T) {
	input := `result = if x > 5 then 1 else 2 end`

	l := NewLexer(input)
	p := NewParser(l)
	program := p.ParseProgram()
	checkParserErrors(t, p)

	if len(program.Statements) != 1 {
		t.Fatalf("program.Statements does not contain 1 statement. got=%d",
			len(program.Statements))
	}

	stmt, ok := program.Statements[0].(*AssignmentStatement)
	if !ok {
		t.Fatalf("program.Statements[0] is not *AssignmentStatement. got=%T",
			program.Statements[0])
	}

	if stmt.Value == nil {
		t.Fatal("stmt.Value is nil")
	}

	ifExp, ok := stmt.Value.(*IfExpression)
	if !ok {
		t.Fatalf("stmt.Value is not *IfExpression. got=%T", stmt.Value)
	}

	if ifExp.Condition == nil {
		t.Fatal("ifExp.Condition is nil")
	}

	if ifExp.Consequence == nil {
		t.Fatal("ifExp.Consequence is nil")
	}

	if ifExp.Alternative == nil {
		t.Fatal("ifExp.Alternative is nil")
	}
}

func TestParseNumericForLoop(t *testing.T) {
	input := `for i = 1, 10 do x = i end`

	l := NewLexer(input)
	p := NewParser(l)
	program := p.ParseProgram()
	checkParserErrors(t, p)

	if len(program.Statements) != 1 {
		t.Fatalf("program.Statements does not contain 1 statement. got=%d",
			len(program.Statements))
	}

	stmt, ok := program.Statements[0].(*NumericForStatement)
	if !ok {
		t.Fatalf("program.Statements[0] is not *NumericForStatement. got=%T",
			program.Statements[0])
	}

	if stmt.VarName == nil {
		t.Fatal("stmt.VarName is nil")
	}

	if stmt.VarName.Value != "i" {
		t.Errorf("stmt.VarName.Value not 'i'. got=%s", stmt.VarName.Value)
	}

	if stmt.Start == nil {
		t.Fatal("stmt.Start is nil")
	}

	if stmt.End == nil {
		t.Fatal("stmt.End is nil")
	}

	if stmt.Body == nil {
		t.Fatal("stmt.Body is nil")
	}
}

func TestParseNumericForLoopWithStep(t *testing.T) {
	input := `for i = 1, 10, 2 do x = i end`

	l := NewLexer(input)
	p := NewParser(l)
	program := p.ParseProgram()
	checkParserErrors(t, p)

	if len(program.Statements) != 1 {
		t.Fatalf("program.Statements does not contain 1 statement. got=%d",
			len(program.Statements))
	}

	stmt, ok := program.Statements[0].(*NumericForStatement)
	if !ok {
		t.Fatalf("program.Statements[0] is not *NumericForStatement. got=%T",
			program.Statements[0])
	}

	if stmt.Step == nil {
		t.Fatal("stmt.Step is nil")
	}
}

func TestParseGenericForLoop(t *testing.T) {
	input := `for k, v in pairs(t) do x = v end`

	l := NewLexer(input)
	p := NewParser(l)
	program := p.ParseProgram()
	checkParserErrors(t, p)

	if len(program.Statements) != 1 {
		t.Fatalf("program.Statements does not contain 1 statement. got=%d",
			len(program.Statements))
	}

	stmt, ok := program.Statements[0].(*GenericForStatement)
	if !ok {
		t.Fatalf("program.Statements[0] is not *GenericForStatement. got=%T",
			program.Statements[0])
	}

	if len(stmt.VarNames) != 2 {
		t.Fatalf("stmt.VarNames does not contain 2 variables. got=%d", len(stmt.VarNames))
	}

	if stmt.VarNames[0].Value != "k" {
		t.Errorf("stmt.VarNames[0].Value not 'k'. got=%s", stmt.VarNames[0].Value)
	}

	if stmt.VarNames[1].Value != "v" {
		t.Errorf("stmt.VarNames[1].Value not 'v'. got=%s", stmt.VarNames[1].Value)
	}

	if stmt.Iterator == nil {
		t.Fatal("stmt.Iterator is nil")
	}

	if stmt.Body == nil {
		t.Fatal("stmt.Body is nil")
	}
}
