package main

import (
	"fmt"
	"testing"
)

func TestLexerBasicTokens(t *testing.T) {
	input := `mut x = 10`

	tests := []struct {
		expectedType    TokenType
		expectedLiteral string
	}{
		{TOKEN_MUT, "mut"},
		{TOKEN_IDENT, "x"},
		{TOKEN_ASSIGN, "="},
		{TOKEN_NUMBER, "10"},
		{TOKEN_EOF, ""},
	}

	lexer := NewLexer(input)

	for i, tt := range tests {
		tok := lexer.NextToken()

		if tok.Type != tt.expectedType {
			t.Fatalf("tests[%d] - tokentype wrong. expected=%q, got=%q",
				i, tt.expectedType, tok.Type)
		}

		if tok.Literal != tt.expectedLiteral {
			t.Fatalf("tests[%d] - literal wrong. expected=%q, got=%q",
				i, tt.expectedLiteral, tok.Literal)
		}
	}
}

func TestLexerOperators(t *testing.T) {
	input := `+ - * / % ^ # & == ~= != < <= > >= ..`

	tests := []struct {
		expectedType    TokenType
		expectedLiteral string
	}{
		{TOKEN_PLUS, "+"},
		{TOKEN_MINUS, "-"},
		{TOKEN_STAR, "*"},
		{TOKEN_SLASH, "/"},
		{TOKEN_PERCENT, "%"},
		{TOKEN_CARET, "^"},
		{TOKEN_HASH, "#"},
		{TOKEN_AMPERSAND, "&"},
		{TOKEN_EQ, "=="},
		{TOKEN_NE, "~="},
		{TOKEN_NE, "!="},
		{TOKEN_LT, "<"},
		{TOKEN_LE, "<="},
		{TOKEN_GT, ">"},
		{TOKEN_GE, ">="},
		{TOKEN_CONCAT, ".."},
		{TOKEN_EOF, ""},
	}

	lexer := NewLexer(input)

	for i, tt := range tests {
		tok := lexer.NextToken()

		if tok.Type != tt.expectedType {
			t.Fatalf("tests[%d] - tokentype wrong. expected=%q, got=%q",
				i, tt.expectedType, tok.Type)
		}

		if tok.Literal != tt.expectedLiteral {
			t.Fatalf("tests[%d] - literal wrong. expected=%q, got=%q",
				i, tt.expectedLiteral, tok.Literal)
		}
	}
}

func TestLexerKeywords(t *testing.T) {
	input := `mut make free function local if then else end for while do return`

	tests := []struct {
		expectedType    TokenType
		expectedLiteral string
	}{
		{TOKEN_MUT, "mut"},
		{TOKEN_MAKE, "make"},
		{TOKEN_FREE, "free"},
		{TOKEN_FUNCTION, "function"},
		{TOKEN_LOCAL, "local"},
		{TOKEN_IF, "if"},
		{TOKEN_THEN, "then"},
		{TOKEN_ELSE, "else"},
		{TOKEN_END, "end"},
		{TOKEN_FOR, "for"},
		{TOKEN_WHILE, "while"},
		{TOKEN_DO, "do"},
		{TOKEN_RETURN, "return"},
		{TOKEN_EOF, ""},
	}

	lexer := NewLexer(input)

	for i, tt := range tests {
		tok := lexer.NextToken()

		if tok.Type != tt.expectedType {
			t.Fatalf("tests[%d] - tokentype wrong. expected=%q, got=%q",
				i, tt.expectedType, tok.Type)
		}

		if tok.Literal != tt.expectedLiteral {
			t.Fatalf("tests[%d] - literal wrong. expected=%q, got=%q",
				i, tt.expectedLiteral, tok.Literal)
		}
	}
}

func TestLexerStrings(t *testing.T) {
	input := `"hello world" 'single quotes' "with\nnewline"`

	tests := []struct {
		expectedType    TokenType
		expectedLiteral string
	}{
		{TOKEN_STRING, "hello world"},
		{TOKEN_STRING, "single quotes"},
		{TOKEN_STRING, "with\nnewline"},
		{TOKEN_EOF, ""},
	}

	lexer := NewLexer(input)

	for i, tt := range tests {
		tok := lexer.NextToken()

		if tok.Type != tt.expectedType {
			t.Fatalf("tests[%d] - tokentype wrong. expected=%q, got=%q",
				i, tt.expectedType, tok.Type)
		}

		if tok.Literal != tt.expectedLiteral {
			t.Fatalf("tests[%d] - literal wrong. expected=%q, got=%q",
				i, tt.expectedLiteral, tok.Literal)
		}
	}
}

func TestLexerNumbers(t *testing.T) {
	input := `123 45.67 1e10 1.5e-3`

	tests := []struct {
		expectedType    TokenType
		expectedLiteral string
	}{
		{TOKEN_NUMBER, "123"},
		{TOKEN_NUMBER, "45.67"},
		{TOKEN_NUMBER, "1e10"},
		{TOKEN_NUMBER, "1.5e-3"},
		{TOKEN_EOF, ""},
	}

	lexer := NewLexer(input)

	for i, tt := range tests {
		tok := lexer.NextToken()

		if tok.Type != tt.expectedType {
			t.Fatalf("tests[%d] - tokentype wrong. expected=%q, got=%q",
				i, tt.expectedType, tok.Type)
		}

		if tok.Literal != tt.expectedLiteral {
			t.Fatalf("tests[%d] - literal wrong. expected=%q, got=%q",
				i, tt.expectedLiteral, tok.Literal)
		}
	}
}

func TestLexerComments(t *testing.T) {
	input := `x = 10 -- this is a comment
	y = 20`

	tests := []struct {
		expectedType    TokenType
		expectedLiteral string
	}{
		{TOKEN_IDENT, "x"},
		{TOKEN_ASSIGN, "="},
		{TOKEN_NUMBER, "10"},
		{TOKEN_IDENT, "y"},
		{TOKEN_ASSIGN, "="},
		{TOKEN_NUMBER, "20"},
		{TOKEN_EOF, ""},
	}

	lexer := NewLexer(input)

	for i, tt := range tests {
		tok := lexer.NextToken()

		if tok.Type != tt.expectedType {
			t.Fatalf("tests[%d] - tokentype wrong. expected=%q, got=%q",
				i, tt.expectedType, tok.Type)
		}

		if tok.Literal != tt.expectedLiteral {
			t.Fatalf("tests[%d] - literal wrong. expected=%q, got=%q",
				i, tt.expectedLiteral, tok.Literal)
		}
	}
}

func TestLexerMultilineComment(t *testing.T) {
	input := `x = 10
	--[[ this is a
	multiline comment ]]
	y = 20`

	tests := []struct {
		expectedType    TokenType
		expectedLiteral string
	}{
		{TOKEN_IDENT, "x"},
		{TOKEN_ASSIGN, "="},
		{TOKEN_NUMBER, "10"},
		{TOKEN_IDENT, "y"},
		{TOKEN_ASSIGN, "="},
		{TOKEN_NUMBER, "20"},
		{TOKEN_EOF, ""},
	}

	lexer := NewLexer(input)

	for i, tt := range tests {
		tok := lexer.NextToken()

		if tok.Type != tt.expectedType {
			t.Fatalf("tests[%d] - tokentype wrong. expected=%q, got=%q",
				i, tt.expectedType, tok.Type)
		}

		if tok.Literal != tt.expectedLiteral {
			t.Fatalf("tests[%d] - literal wrong. expected=%q, got=%q",
				i, tt.expectedLiteral, tok.Literal)
		}
	}
}

func TestLexerPointerOperations(t *testing.T) {
	input := `ptr = make(int, 100)
	*ptr = 42
	free(ptr)`

	tests := []struct {
		expectedType    TokenType
		expectedLiteral string
	}{
		{TOKEN_IDENT, "ptr"},
		{TOKEN_ASSIGN, "="},
		{TOKEN_MAKE, "make"},
		{TOKEN_LPAREN, "("},
		{TOKEN_IDENT, "int"},
		{TOKEN_COMMA, ","},
		{TOKEN_NUMBER, "100"},
		{TOKEN_RPAREN, ")"},
		{TOKEN_STAR, "*"},
		{TOKEN_IDENT, "ptr"},
		{TOKEN_ASSIGN, "="},
		{TOKEN_NUMBER, "42"},
		{TOKEN_FREE, "free"},
		{TOKEN_LPAREN, "("},
		{TOKEN_IDENT, "ptr"},
		{TOKEN_RPAREN, ")"},
		{TOKEN_EOF, ""},
	}

	lexer := NewLexer(input)

	for i, tt := range tests {
		tok := lexer.NextToken()

		if tok.Type != tt.expectedType {
			t.Fatalf("tests[%d] - tokentype wrong. expected=%q, got=%q",
				i, tt.expectedType, tok.Type)
		}

		if tok.Literal != tt.expectedLiteral {
			t.Fatalf("tests[%d] - literal wrong. expected=%q, got=%q",
				i, tt.expectedLiteral, tok.Literal)
		}
	}
}

func TestLexerComplexExpression(t *testing.T) {
	input := `mut x = 10
	y = 20 + x * 2
	if x == 10 then
		print("x is ten")
	end`

	tests := []struct {
		expectedType    TokenType
		expectedLiteral string
	}{
		{TOKEN_MUT, "mut"},
		{TOKEN_IDENT, "x"},
		{TOKEN_ASSIGN, "="},
		{TOKEN_NUMBER, "10"},
		{TOKEN_IDENT, "y"},
		{TOKEN_ASSIGN, "="},
		{TOKEN_NUMBER, "20"},
		{TOKEN_PLUS, "+"},
		{TOKEN_IDENT, "x"},
		{TOKEN_STAR, "*"},
		{TOKEN_NUMBER, "2"},
		{TOKEN_IF, "if"},
		{TOKEN_IDENT, "x"},
		{TOKEN_EQ, "=="},
		{TOKEN_NUMBER, "10"},
		{TOKEN_THEN, "then"},
		{TOKEN_IDENT, "print"},
		{TOKEN_LPAREN, "("},
		{TOKEN_STRING, "x is ten"},
		{TOKEN_RPAREN, ")"},
		{TOKEN_END, "end"},
		{TOKEN_EOF, ""},
	}

	lexer := NewLexer(input)

	for i, tt := range tests {
		tok := lexer.NextToken()

		if tok.Type != tt.expectedType {
			t.Fatalf("tests[%d] - tokentype wrong. expected=%q, got=%q",
				i, tt.expectedType, tok.Type)
		}

		if tok.Literal != tt.expectedLiteral {
			t.Fatalf("tests[%d] - literal wrong. expected=%q, got=%q",
				i, tt.expectedLiteral, tok.Literal)
		}
	}
}

func TestLexerLineAndColumnTracking(t *testing.T) {
	input := `x = 10
y = 20`

	lexer := NewLexer(input)

	tok := lexer.NextToken()
	if tok.Line != 1 || tok.Column != 1 {
		t.Fatalf("Expected token at line 1, col 1, got line %d, col %d", tok.Line, tok.Column)
	}

	lexer.NextToken() // =
	lexer.NextToken() // 10

	tok = lexer.NextToken()
	if tok.Line != 2 || tok.Column != 1 {
		t.Fatalf("Expected token at line 2, col 1, got line %d, col %d", tok.Line, tok.Column)
	}
}

// Example function to print all tokens (useful for debugging)
func ExampleLexer() {
	input := `
	mut x = 10
	y = 20 + x * 2
	ptr = make(int, 100)
	*ptr = 42
	free(ptr)
	
	-- This is a comment
	name = "Alice"
	if x == 10 then
		print("x is ten")
	end
	`

	lexer := NewLexer(input)

	fmt.Println("Tokenizing Lua code:")
	fmt.Println("=====================")

	for {
		tok := lexer.NextToken()
		fmt.Printf("Line %d, Col %d: %-12s '%s'\n", tok.Line, tok.Column, tok.Type, tok.Literal)

		if tok.Type == TOKEN_EOF {
			break
		}
	}
}
