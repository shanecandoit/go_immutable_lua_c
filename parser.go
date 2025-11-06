package main

import "fmt"

// Node represents a node in the abstract syntax tree
type Node interface {
	TokenLiteral() string
	String() string
}

// Statement represents a statement node
type Statement interface {
	Node
	statementNode()
}

// Expression represents an expression node
type Expression interface {
	Node
	expressionNode()
}

// Program represents the root node of the AST
type Program struct {
	Statements []Statement
}

func (p *Program) TokenLiteral() string {
	if len(p.Statements) > 0 {
		return p.Statements[0].TokenLiteral()
	}
	return ""
}

func (p *Program) String() string {
	var out string
	for _, s := range p.Statements {
		out += s.String()
	}
	return out
}

// AssignmentStatement represents a variable assignment
// Examples: x = 10, mut y = 20
type AssignmentStatement struct {
	Token   Token       // The first token (IDENT or MUT)
	Mutable bool        // Whether the variable is mutable
	Name    *Identifier // The variable name
	Value   Expression  // The value expression
}

func (as *AssignmentStatement) statementNode()       {}
func (as *AssignmentStatement) TokenLiteral() string { return as.Token.Literal }
func (as *AssignmentStatement) String() string {
	var out string
	if as.Mutable {
		out += "mut "
	}
	out += as.Name.String()
	out += " = "
	if as.Value != nil {
		out += as.Value.String()
	}
	return out
}

// ExpressionStatement represents an expression used as a statement
type ExpressionStatement struct {
	Token      Token // The first token of the expression
	Expression Expression
}

func (es *ExpressionStatement) statementNode()       {}
func (es *ExpressionStatement) TokenLiteral() string { return es.Token.Literal }
func (es *ExpressionStatement) String() string {
	if es.Expression != nil {
		return es.Expression.String()
	}
	return ""
}

// Identifier represents an identifier
type Identifier struct {
	Token Token  // The TOKEN_IDENT token
	Value string // The identifier name
}

func (i *Identifier) expressionNode()      {}
func (i *Identifier) TokenLiteral() string { return i.Token.Literal }
func (i *Identifier) String() string       { return i.Value }

// IntegerLiteral represents an integer literal
type IntegerLiteral struct {
	Token Token
	Value string
}

func (il *IntegerLiteral) expressionNode()      {}
func (il *IntegerLiteral) TokenLiteral() string { return il.Token.Literal }
func (il *IntegerLiteral) String() string       { return il.Value }

// FloatLiteral represents a float literal
type FloatLiteral struct {
	Token Token
	Value string
}

func (fl *FloatLiteral) expressionNode()      {}
func (fl *FloatLiteral) TokenLiteral() string { return fl.Token.Literal }
func (fl *FloatLiteral) String() string       { return fl.Value }

// StringLiteral represents a string literal
type StringLiteral struct {
	Token Token
	Value string
}

func (sl *StringLiteral) expressionNode()      {}
func (sl *StringLiteral) TokenLiteral() string { return sl.Token.Literal }
func (sl *StringLiteral) String() string       { return "\"" + sl.Value + "\"" }

// BooleanLiteral represents a boolean literal
type BooleanLiteral struct {
	Token Token
	Value bool
}

func (bl *BooleanLiteral) expressionNode()      {}
func (bl *BooleanLiteral) TokenLiteral() string { return bl.Token.Literal }
func (bl *BooleanLiteral) String() string {
	if bl.Value {
		return "true"
	}
	return "false"
}

// NilLiteral represents a nil literal
type NilLiteral struct {
	Token Token
}

func (nl *NilLiteral) expressionNode()      {}
func (nl *NilLiteral) TokenLiteral() string { return nl.Token.Literal }
func (nl *NilLiteral) String() string       { return "nil" }

// InfixExpression represents a binary operation
type InfixExpression struct {
	Token    Token // The operator token
	Left     Expression
	Operator string
	Right    Expression
}

func (ie *InfixExpression) expressionNode()      {}
func (ie *InfixExpression) TokenLiteral() string { return ie.Token.Literal }
func (ie *InfixExpression) String() string {
	return "(" + ie.Left.String() + " " + ie.Operator + " " + ie.Right.String() + ")"
}

// PrefixExpression represents a unary operation
type PrefixExpression struct {
	Token    Token // The operator token (e.g., *, &, -, not)
	Operator string
	Right    Expression
}

func (pe *PrefixExpression) expressionNode()      {}
func (pe *PrefixExpression) TokenLiteral() string { return pe.Token.Literal }
func (pe *PrefixExpression) String() string {
	return "(" + pe.Operator + pe.Right.String() + ")"
}

// CallExpression represents a function call
type CallExpression struct {
	Token     Token      // The '(' token
	Function  Expression // Identifier or FunctionLiteral
	Arguments []Expression
}

func (ce *CallExpression) expressionNode()      {}
func (ce *CallExpression) TokenLiteral() string { return ce.Token.Literal }
func (ce *CallExpression) String() string {
	var args string
	for i, arg := range ce.Arguments {
		if i > 0 {
			args += ", "
		}
		args += arg.String()
	}
	return ce.Function.String() + "(" + args + ")"
}

// Parser represents the parser
type Parser struct {
	lexer     *Lexer
	curToken  Token
	peekToken Token
	errors    []string
}

// NewParser creates a new parser
func NewParser(l *Lexer) *Parser {
	p := &Parser{
		lexer:  l,
		errors: []string{},
	}

	// Read two tokens, so curToken and peekToken are both set
	p.nextToken()
	p.nextToken()

	return p
}

// Errors returns the parser errors
func (p *Parser) Errors() []string {
	return p.errors
}

// nextToken advances the parser tokens
func (p *Parser) nextToken() {
	p.curToken = p.peekToken
	p.peekToken = p.lexer.NextToken()
}

// curTokenIs checks if current token matches the given type
func (p *Parser) curTokenIs(t TokenType) bool {
	return p.curToken.Type == t
}

// peekTokenIs checks if peek token matches the given type
func (p *Parser) peekTokenIs(t TokenType) bool {
	return p.peekToken.Type == t
}

// expectPeek advances if peek token matches, otherwise adds error
func (p *Parser) expectPeek(t TokenType) bool {
	if p.peekTokenIs(t) {
		p.nextToken()
		return true
	}
	p.peekError(t)
	return false
}

// peekError adds a peek error
func (p *Parser) peekError(t TokenType) {
	msg := fmt.Sprintf("Line %d, Col %d: expected next token to be %s, got %s instead",
		p.peekToken.Line, p.peekToken.Column, t, p.peekToken.Type)
	p.errors = append(p.errors, msg)
}

// ParseProgram parses the entire program
func (p *Parser) ParseProgram() *Program {
	program := &Program{}
	program.Statements = []Statement{}

	for !p.curTokenIs(TOKEN_EOF) {
		stmt := p.parseStatement()
		if stmt != nil {
			program.Statements = append(program.Statements, stmt)
		}
		p.nextToken()
	}

	return program
}

// parseStatement parses a statement
func (p *Parser) parseStatement() Statement {
	switch p.curToken.Type {
	case TOKEN_MUT:
		return p.parseAssignmentStatement(true)
	case TOKEN_IDENT:
		// Could be assignment or expression statement
		if p.peekTokenIs(TOKEN_ASSIGN) {
			return p.parseAssignmentStatement(false)
		}
		// Parse as expression statement
		return p.parseExpressionStatement()
	default:
		// Try to parse as expression statement
		return p.parseExpressionStatement()
	}
}

// parseExpressionStatement parses an expression statement
func (p *Parser) parseExpressionStatement() *ExpressionStatement {
	stmt := &ExpressionStatement{
		Token: p.curToken,
	}

	stmt.Expression = p.parseExpression(LOWEST)

	return stmt
}

// parseAssignmentStatement parses a variable assignment
func (p *Parser) parseAssignmentStatement(mutable bool) *AssignmentStatement {
	stmt := &AssignmentStatement{
		Token:   p.curToken,
		Mutable: mutable,
	}

	// If mutable, expect identifier next
	if mutable {
		if !p.expectPeek(TOKEN_IDENT) {
			return nil
		}
	}

	// Current token should be the identifier
	stmt.Name = &Identifier{
		Token: p.curToken,
		Value: p.curToken.Literal,
	}

	// Expect =
	if !p.expectPeek(TOKEN_ASSIGN) {
		return nil
	}

	// Move past =
	p.nextToken()

	// Parse the value expression
	stmt.Value = p.parseExpression(LOWEST)

	return stmt
}

// Operator precedence
const (
	_ int = iota
	LOWEST
	OR          // or
	AND         // and
	EQUALS      // == or ~=
	LESSGREATER // > or <
	SUM         // + or -
	PRODUCT     // * or /
	POWER       // ^
	PREFIX      // -x or not x or *ptr or &ptr
	CALL        // myFunction(x)
)

var precedences = map[TokenType]int{
	TOKEN_OR:      OR,
	TOKEN_AND:     AND,
	TOKEN_EQ:      EQUALS,
	TOKEN_NE:      EQUALS,
	TOKEN_LT:      LESSGREATER,
	TOKEN_LE:      LESSGREATER,
	TOKEN_GT:      LESSGREATER,
	TOKEN_GE:      LESSGREATER,
	TOKEN_PLUS:    SUM,
	TOKEN_MINUS:   SUM,
	TOKEN_CONCAT:  SUM,
	TOKEN_SLASH:   PRODUCT,
	TOKEN_STAR:    PRODUCT,
	TOKEN_PERCENT: PRODUCT,
	TOKEN_CARET:   POWER,
	TOKEN_LPAREN:  CALL,
}

func (p *Parser) peekPrecedence() int {
	if prec, ok := precedences[p.peekToken.Type]; ok {
		return prec
	}
	return LOWEST
}

func (p *Parser) curPrecedence() int {
	if prec, ok := precedences[p.curToken.Type]; ok {
		return prec
	}
	return LOWEST
}

// parseExpression parses an expression
func (p *Parser) parseExpression(precedence int) Expression {
	// Parse prefix
	var leftExp Expression

	switch p.curToken.Type {
	case TOKEN_IDENT:
		leftExp = &Identifier{Token: p.curToken, Value: p.curToken.Literal}
	case TOKEN_NUMBER:
		leftExp = p.parseNumberLiteral()
	case TOKEN_STRING:
		leftExp = &StringLiteral{Token: p.curToken, Value: p.curToken.Literal}
	case TOKEN_TRUE:
		leftExp = &BooleanLiteral{Token: p.curToken, Value: true}
	case TOKEN_FALSE:
		leftExp = &BooleanLiteral{Token: p.curToken, Value: false}
	case TOKEN_NIL:
		leftExp = &NilLiteral{Token: p.curToken}
	case TOKEN_MINUS, TOKEN_NOT, TOKEN_STAR, TOKEN_AMPERSAND, TOKEN_HASH:
		leftExp = p.parsePrefixExpression()
	case TOKEN_LPAREN:
		p.nextToken()
		leftExp = p.parseExpression(LOWEST)
		if !p.expectPeek(TOKEN_RPAREN) {
			return nil
		}
	// Keywords that can be used as identifiers in expressions (function names, etc.)
	case TOKEN_MAKE, TOKEN_FREE, TOKEN_FUNCTION, TOKEN_LOCAL,
		TOKEN_IF, TOKEN_THEN, TOKEN_ELSE, TOKEN_ELSEIF, TOKEN_END,
		TOKEN_FOR, TOKEN_WHILE, TOKEN_DO, TOKEN_REPEAT, TOKEN_UNTIL,
		TOKEN_RETURN, TOKEN_BREAK, TOKEN_IN:
		// Treat keywords as identifiers in expression context
		leftExp = &Identifier{Token: p.curToken, Value: p.curToken.Literal}
	default:
		p.noPrefixParseFnError(p.curToken.Type)
		return nil
	}

	// Parse infix
	for !p.peekTokenIs(TOKEN_EOF) && precedence < p.peekPrecedence() {
		switch p.peekToken.Type {
		case TOKEN_PLUS, TOKEN_MINUS, TOKEN_STAR, TOKEN_SLASH, TOKEN_PERCENT,
			TOKEN_EQ, TOKEN_NE, TOKEN_LT, TOKEN_LE, TOKEN_GT, TOKEN_GE,
			TOKEN_AND, TOKEN_OR, TOKEN_CONCAT, TOKEN_CARET:
			p.nextToken()
			leftExp = p.parseInfixExpression(leftExp)
		case TOKEN_LPAREN:
			p.nextToken()
			leftExp = p.parseCallExpression(leftExp)
		default:
			return leftExp
		}
	}

	return leftExp
}

// parseNumberLiteral parses a number literal (int or float)
func (p *Parser) parseNumberLiteral() Expression {
	// For simplicity, we'll treat all numbers as strings for now
	// In a real implementation, you'd parse them properly
	return &IntegerLiteral{Token: p.curToken, Value: p.curToken.Literal}
}

// parsePrefixExpression parses a prefix expression
func (p *Parser) parsePrefixExpression() Expression {
	expression := &PrefixExpression{
		Token:    p.curToken,
		Operator: p.curToken.Literal,
	}

	p.nextToken()

	expression.Right = p.parseExpression(PREFIX)

	return expression
}

// parseInfixExpression parses an infix expression
func (p *Parser) parseInfixExpression(left Expression) Expression {
	expression := &InfixExpression{
		Token:    p.curToken,
		Operator: p.curToken.Literal,
		Left:     left,
	}

	precedence := p.curPrecedence()
	p.nextToken()
	expression.Right = p.parseExpression(precedence)

	return expression
}

// parseCallExpression parses a function call
func (p *Parser) parseCallExpression(function Expression) Expression {
	exp := &CallExpression{Token: p.curToken, Function: function}
	exp.Arguments = p.parseCallArguments()
	return exp
}

// parseCallArguments parses function call arguments
func (p *Parser) parseCallArguments() []Expression {
	args := []Expression{}

	if p.peekTokenIs(TOKEN_RPAREN) {
		p.nextToken()
		return args
	}

	p.nextToken()
	args = append(args, p.parseExpression(LOWEST))

	for p.peekTokenIs(TOKEN_COMMA) {
		p.nextToken()
		p.nextToken()
		args = append(args, p.parseExpression(LOWEST))
	}

	if !p.expectPeek(TOKEN_RPAREN) {
		return nil
	}

	return args
}

// noPrefixParseFnError adds an error for missing prefix parse function
func (p *Parser) noPrefixParseFnError(t TokenType) {
	msg := fmt.Sprintf("Line %d, Col %d: no prefix parse function for %s found",
		p.curToken.Line, p.curToken.Column, t)
	p.errors = append(p.errors, msg)
}
