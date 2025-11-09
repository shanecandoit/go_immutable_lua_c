package main

import (
	"fmt"
	"strings"
	"unicode"
)

// TokenType represents the type of token
type TokenType int

const (
	// Special tokens
	TOKEN_EOF TokenType = iota
	TOKEN_ILLEGAL

	// Identifiers and literals
	TOKEN_IDENT  // variable names
	TOKEN_NUMBER // 123, 45.67
	TOKEN_STRING // "hello", 'world'

	// Keywords
	TOKEN_MUT      // mut
	TOKEN_MAKE     // make
	TOKEN_FREE     // free
	TOKEN_FUNCTION // function
	TOKEN_LOCAL    // local
	TOKEN_IF       // if
	TOKEN_THEN     // then
	TOKEN_ELSE     // else
	TOKEN_ELSEIF   // elseif
	TOKEN_END      // end
	TOKEN_FOR      // for
	TOKEN_WHILE    // while
	TOKEN_DO       // do
	TOKEN_REPEAT   // repeat
	TOKEN_UNTIL    // until
	TOKEN_RETURN   // return
	TOKEN_BREAK    // break
	TOKEN_NIL      // nil
	TOKEN_TRUE     // true
	TOKEN_FALSE    // false
	TOKEN_AND      // and
	TOKEN_OR       // or
	TOKEN_NOT      // not
	TOKEN_IN       // in

	// Operators
	TOKEN_PLUS      // +
	TOKEN_MINUS     // -
	TOKEN_STAR      // *
	TOKEN_SLASH     // /
	TOKEN_PERCENT   // %
	TOKEN_CARET     // ^
	TOKEN_HASH      // #
	TOKEN_AMPERSAND // &
	TOKEN_ASSIGN    // =
	TOKEN_EQ        // ==
	TOKEN_NE        // ~= or !=
	TOKEN_LT        // <
	TOKEN_LE        // <=
	TOKEN_GT        // >
	TOKEN_GE        // >=
	TOKEN_CONCAT    // ..

	// Delimiters
	TOKEN_LPAREN    // (
	TOKEN_RPAREN    // )
	TOKEN_LBRACE    // {
	TOKEN_RBRACE    // }
	TOKEN_LBRACKET  // [
	TOKEN_RBRACKET  // ]
	TOKEN_SEMICOLON // ;
	TOKEN_COMMA     // ,
	TOKEN_DOT       // .
	TOKEN_COLON     // :
)

// Token represents a lexical token
type Token struct {
	Type    TokenType
	Literal string
	Line    int
	Column  int
}

// Lexer performs lexical analysis
type Lexer struct {
	input        string
	position     int  // current position in input
	readPosition int  // current reading position in input (after current char)
	ch           byte // current char under examination
	line         int
	column       int
}

var keywords = map[string]TokenType{
	"mut":      TOKEN_MUT,
	"make":     TOKEN_MAKE,
	"free":     TOKEN_FREE,
	"function": TOKEN_FUNCTION,
	"local":    TOKEN_LOCAL,
	"if":       TOKEN_IF,
	"then":     TOKEN_THEN,
	"else":     TOKEN_ELSE,
	"elseif":   TOKEN_ELSEIF,
	"end":      TOKEN_END,
	"for":      TOKEN_FOR,
	"while":    TOKEN_WHILE,
	"do":       TOKEN_DO,
	"repeat":   TOKEN_REPEAT,
	"until":    TOKEN_UNTIL,
	"return":   TOKEN_RETURN,
	"break":    TOKEN_BREAK,
	"nil":      TOKEN_NIL,
	"true":     TOKEN_TRUE,
	"false":    TOKEN_FALSE,
	"and":      TOKEN_AND,
	"or":       TOKEN_OR,
	"not":      TOKEN_NOT,
	"in":       TOKEN_IN,
}

// NewLexer creates a new lexer for the given input
func NewLexer(input string) *Lexer {
	l := &Lexer{
		input:  input,
		line:   1,
		column: 0,
	}
	l.readChar()
	return l
}

// readChar reads the next character and advances position
func (l *Lexer) readChar() {
	if l.readPosition >= len(l.input) {
		l.ch = 0 // ASCII code for NUL, signifies EOF
	} else {
		l.ch = l.input[l.readPosition]
	}
	l.position = l.readPosition
	l.readPosition++
	l.column++

	if l.ch == '\n' {
		l.line++
		l.column = 0
	}
}

// peekChar looks at the next character without advancing
func (l *Lexer) peekChar() byte {
	if l.readPosition >= len(l.input) {
		return 0
	}
	return l.input[l.readPosition]
}

// skipWhitespace skips whitespace characters
func (l *Lexer) skipWhitespace() {
	for l.ch == ' ' || l.ch == '\t' || l.ch == '\n' || l.ch == '\r' {
		l.readChar()
	}
}

// skipComment skips single-line and multi-line comments
func (l *Lexer) skipComment() {
	if l.ch == '-' && l.peekChar() == '-' {
		l.readChar() // consume second -
		l.readChar()

		// Check for multi-line comment [[
		if l.ch == '[' && l.peekChar() == '[' {
			l.readChar() // consume first [
			l.readChar() // consume second [

			// Read until we find ]]
			for {
				if l.ch == 0 {
					break
				}
				if l.ch == ']' && l.peekChar() == ']' {
					l.readChar() // consume first ]
					l.readChar() // consume second ]
					break
				}
				l.readChar()
			}
		} else {
			// Single-line comment
			for l.ch != '\n' && l.ch != 0 {
				l.readChar()
			}
		}
	}
}

// readIdentifier reads an identifier or keyword
func (l *Lexer) readIdentifier() string {
	position := l.position
	for isLetter(l.ch) || isDigit(l.ch) || l.ch == '_' {
		l.readChar()
	}
	return l.input[position:l.position]
}

// readNumber reads a number (integer or float)
func (l *Lexer) readNumber() string {
	position := l.position
	hasDot := false

	for isDigit(l.ch) || (l.ch == '.' && !hasDot) {
		if l.ch == '.' {
			// Check if next char is digit (to distinguish from .. operator)
			if !isDigit(l.peekChar()) {
				break
			}
			hasDot = true
		}
		l.readChar()
	}

	// Handle scientific notation (e.g., 1e10, 1.5e-3)
	if l.ch == 'e' || l.ch == 'E' {
		l.readChar()
		if l.ch == '+' || l.ch == '-' {
			l.readChar()
		}
		for isDigit(l.ch) {
			l.readChar()
		}
	}

	num := l.input[position:l.position]
	fmt.Printf("readNumber: %s\n", num)
	return num
}

// readString reads a string literal
func (l *Lexer) readString(quote byte) string {
	var result strings.Builder
	l.readChar() // consume opening quote

	for l.ch != quote && l.ch != 0 {
		if l.ch == '\\' {
			l.readChar()
			// Handle escape sequences
			switch l.ch {
			case 'n':
				result.WriteByte('\n')
			case 't':
				result.WriteByte('\t')
			case 'r':
				result.WriteByte('\r')
			case '\\':
				result.WriteByte('\\')
			case '"':
				result.WriteByte('"')
			case '\'':
				result.WriteByte('\'')
			default:
				result.WriteByte(l.ch)
			}
		} else {
			result.WriteByte(l.ch)
		}
		l.readChar()
	}

	if l.ch == quote {
		l.readChar() // consume closing quote
	}

	str := result.String()
	fmt.Printf("readString: %s\n", str)
	return str
}

// NextToken returns the next token from the input
func (l *Lexer) NextToken() Token {
	var tok Token

	l.skipWhitespace()

	// Skip comments
	for l.ch == '-' && l.peekChar() == '-' {
		l.skipComment()
		l.skipWhitespace()
	}

	tok.Line = l.line
	tok.Column = l.column

	switch l.ch {
	case '=':
		if l.peekChar() == '=' {
			ch := l.ch
			l.readChar()
			tok = Token{Type: TOKEN_EQ, Literal: string(ch) + string(l.ch), Line: tok.Line, Column: tok.Column}
		} else {
			tok = Token{Type: TOKEN_ASSIGN, Literal: string(l.ch), Line: tok.Line, Column: tok.Column}
		}
	case '+':
		tok = Token{Type: TOKEN_PLUS, Literal: string(l.ch), Line: tok.Line, Column: tok.Column}
	case '-':
		tok = Token{Type: TOKEN_MINUS, Literal: string(l.ch), Line: tok.Line, Column: tok.Column}
	case '*':
		tok = Token{Type: TOKEN_STAR, Literal: string(l.ch), Line: tok.Line, Column: tok.Column}
	case '/':
		tok = Token{Type: TOKEN_SLASH, Literal: string(l.ch), Line: tok.Line, Column: tok.Column}
	case '%':
		tok = Token{Type: TOKEN_PERCENT, Literal: string(l.ch), Line: tok.Line, Column: tok.Column}
	case '^':
		tok = Token{Type: TOKEN_CARET, Literal: string(l.ch), Line: tok.Line, Column: tok.Column}
	case '#':
		tok = Token{Type: TOKEN_HASH, Literal: string(l.ch), Line: tok.Line, Column: tok.Column}
	case '&':
		tok = Token{Type: TOKEN_AMPERSAND, Literal: string(l.ch), Line: tok.Line, Column: tok.Column}
	case '~':
		if l.peekChar() == '=' {
			ch := l.ch
			l.readChar()
			tok = Token{Type: TOKEN_NE, Literal: string(ch) + string(l.ch), Line: tok.Line, Column: tok.Column}
		} else {
			tok = Token{Type: TOKEN_ILLEGAL, Literal: string(l.ch), Line: tok.Line, Column: tok.Column}
		}
	case '!':
		if l.peekChar() == '=' {
			ch := l.ch
			l.readChar()
			tok = Token{Type: TOKEN_NE, Literal: string(ch) + string(l.ch), Line: tok.Line, Column: tok.Column}
		} else {
			tok = Token{Type: TOKEN_ILLEGAL, Literal: string(l.ch), Line: tok.Line, Column: tok.Column}
		}
	case '<':
		if l.peekChar() == '=' {
			ch := l.ch
			l.readChar()
			tok = Token{Type: TOKEN_LE, Literal: string(ch) + string(l.ch), Line: tok.Line, Column: tok.Column}
		} else {
			tok = Token{Type: TOKEN_LT, Literal: string(l.ch), Line: tok.Line, Column: tok.Column}
		}
	case '>':
		if l.peekChar() == '=' {
			ch := l.ch
			l.readChar()
			tok = Token{Type: TOKEN_GE, Literal: string(ch) + string(l.ch), Line: tok.Line, Column: tok.Column}
		} else {
			tok = Token{Type: TOKEN_GT, Literal: string(l.ch), Line: tok.Line, Column: tok.Column}
		}
	case '.':
		if l.peekChar() == '.' {
			ch := l.ch
			l.readChar()
			tok = Token{Type: TOKEN_CONCAT, Literal: string(ch) + string(l.ch), Line: tok.Line, Column: tok.Column}
		} else {
			tok = Token{Type: TOKEN_DOT, Literal: string(l.ch), Line: tok.Line, Column: tok.Column}
		}
	case '(':
		tok = Token{Type: TOKEN_LPAREN, Literal: string(l.ch), Line: tok.Line, Column: tok.Column}
	case ')':
		tok = Token{Type: TOKEN_RPAREN, Literal: string(l.ch), Line: tok.Line, Column: tok.Column}
	case '{':
		tok = Token{Type: TOKEN_LBRACE, Literal: string(l.ch), Line: tok.Line, Column: tok.Column}
	case '}':
		tok = Token{Type: TOKEN_RBRACE, Literal: string(l.ch), Line: tok.Line, Column: tok.Column}
	case '[':
		tok = Token{Type: TOKEN_LBRACKET, Literal: string(l.ch), Line: tok.Line, Column: tok.Column}
	case ']':
		tok = Token{Type: TOKEN_RBRACKET, Literal: string(l.ch), Line: tok.Line, Column: tok.Column}
	case ';':
		tok = Token{Type: TOKEN_SEMICOLON, Literal: string(l.ch), Line: tok.Line, Column: tok.Column}
	case ',':
		tok = Token{Type: TOKEN_COMMA, Literal: string(l.ch), Line: tok.Line, Column: tok.Column}
	case ':':
		tok = Token{Type: TOKEN_COLON, Literal: string(l.ch), Line: tok.Line, Column: tok.Column}
	case '"', '\'':
		tok.Type = TOKEN_STRING
		tok.Literal = l.readString(l.ch)
		return tok
	case 0:
		tok.Literal = ""
		tok.Type = TOKEN_EOF
	default:
		if isLetter(l.ch) || l.ch == '_' {
			tok.Literal = l.readIdentifier()
			tok.Type = lookupIdent(tok.Literal)
			return tok
		} else if isDigit(l.ch) {
			tok.Type = TOKEN_NUMBER
			tok.Literal = l.readNumber()
			return tok
		} else {
			tok = Token{Type: TOKEN_ILLEGAL, Literal: string(l.ch), Line: tok.Line, Column: tok.Column}
		}
	}

	l.readChar()
	return tok
}

// lookupIdent checks if identifier is a keyword
func lookupIdent(ident string) TokenType {
	if tok, ok := keywords[ident]; ok {
		return tok
	}
	return TOKEN_IDENT
}

// Helper functions
func isLetter(ch byte) bool {
	return unicode.IsLetter(rune(ch))
}

func isDigit(ch byte) bool {
	return '0' <= ch && ch <= '9'
}

// String returns a string representation of the token type
func (tt TokenType) String() string {
	names := map[TokenType]string{
		TOKEN_EOF:       "EOF",
		TOKEN_ILLEGAL:   "ILLEGAL",
		TOKEN_IDENT:     "IDENT",
		TOKEN_NUMBER:    "NUMBER",
		TOKEN_STRING:    "STRING",
		TOKEN_MUT:       "MUT",
		TOKEN_MAKE:      "MAKE",
		TOKEN_FREE:      "FREE",
		TOKEN_FUNCTION:  "FUNCTION",
		TOKEN_LOCAL:     "LOCAL",
		TOKEN_IF:        "IF",
		TOKEN_THEN:      "THEN",
		TOKEN_ELSE:      "ELSE",
		TOKEN_ELSEIF:    "ELSEIF",
		TOKEN_END:       "END",
		TOKEN_FOR:       "FOR",
		TOKEN_WHILE:     "WHILE",
		TOKEN_DO:        "DO",
		TOKEN_REPEAT:    "REPEAT",
		TOKEN_UNTIL:     "UNTIL",
		TOKEN_RETURN:    "RETURN",
		TOKEN_BREAK:     "BREAK",
		TOKEN_NIL:       "NIL",
		TOKEN_TRUE:      "TRUE",
		TOKEN_FALSE:     "FALSE",
		TOKEN_AND:       "AND",
		TOKEN_OR:        "OR",
		TOKEN_NOT:       "NOT",
		TOKEN_IN:        "IN",
		TOKEN_PLUS:      "PLUS",
		TOKEN_MINUS:     "MINUS",
		TOKEN_STAR:      "STAR",
		TOKEN_SLASH:     "SLASH",
		TOKEN_PERCENT:   "PERCENT",
		TOKEN_CARET:     "CARET",
		TOKEN_HASH:      "HASH",
		TOKEN_AMPERSAND: "AMPERSAND",
		TOKEN_ASSIGN:    "ASSIGN",
		TOKEN_EQ:        "EQ",
		TOKEN_NE:        "NE",
		TOKEN_LT:        "LT",
		TOKEN_LE:        "LE",
		TOKEN_GT:        "GT",
		TOKEN_GE:        "GE",
		TOKEN_CONCAT:    "CONCAT",
		TOKEN_LPAREN:    "LPAREN",
		TOKEN_RPAREN:    "RPAREN",
		TOKEN_LBRACE:    "LBRACE",
		TOKEN_RBRACE:    "RBRACE",
		TOKEN_LBRACKET:  "LBRACKET",
		TOKEN_RBRACKET:  "RBRACKET",
		TOKEN_SEMICOLON: "SEMICOLON",
		TOKEN_COMMA:     "COMMA",
		TOKEN_DOT:       "DOT",
		TOKEN_COLON:     "COLON",
	}
	if name, ok := names[tt]; ok {
		return name
	}
	return "UNKNOWN"
}
