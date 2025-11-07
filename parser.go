package main

import (
	"fmt"
)

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
	case TOKEN_IF:
		return p.parseIfStatement()
	case TOKEN_FOR:
		return p.parseForStatement()
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

// parseIfStatement parses an if-then-else statement
func (p *Parser) parseIfStatement() *IfStatement {
	stmt := &IfStatement{
		Token: p.curToken, // 'if'
	}

	// Move to condition
	p.nextToken()

	// Parse condition
	stmt.Condition = p.parseExpression(LOWEST)

	// Expect 'then'
	if !p.expectPeek(TOKEN_THEN) {
		return nil
	}

	// Move past 'then'
	p.nextToken()

	// Parse consequence block
	stmt.Consequence = p.parseBlockStatement(TOKEN_ELSEIF, TOKEN_ELSE, TOKEN_END)

	// Handle elseif clauses
	for p.curTokenIs(TOKEN_ELSEIF) {
		elseif := &ElseIfClause{
			Token: p.curToken,
		}

		// Move to condition
		p.nextToken()

		// Parse condition
		elseif.Condition = p.parseExpression(LOWEST)

		// Expect 'then'
		if !p.expectPeek(TOKEN_THEN) {
			return nil
		}

		// Move past 'then'
		p.nextToken()

		// Parse consequence block
		elseif.Consequence = p.parseBlockStatement(TOKEN_ELSEIF, TOKEN_ELSE, TOKEN_END)

		stmt.Alternatives = append(stmt.Alternatives, elseif)
	}

	// Handle else clause
	if p.curTokenIs(TOKEN_ELSE) {
		// Move past 'else'
		p.nextToken()

		// Parse alternative block
		stmt.Alternative = p.parseBlockStatement(TOKEN_END)
	}

	// Expect 'end'
	if !p.curTokenIs(TOKEN_END) {
		return nil
	}

	return stmt
}

// parseIfExpression parses an if expression and returns an IfExpression
func (p *Parser) parseIfExpression() Expression {
	expr := &IfExpression{Token: p.curToken}

	// Move to condition
	p.nextToken()

	// Parse condition
	expr.Condition = p.parseExpression(LOWEST)

	// Expect 'then'
	if !p.expectPeek(TOKEN_THEN) {
		return nil
	}

	// Move past 'then'
	p.nextToken()

	// Parse consequence block
	expr.Consequence = p.parseBlockStatement(TOKEN_ELSEIF, TOKEN_ELSE, TOKEN_END)

	// Handle elseif clauses
	for p.curTokenIs(TOKEN_ELSEIF) {
		elseif := &ElseIfClause{Token: p.curToken}

		// Move to condition
		p.nextToken()

		// Parse condition
		elseif.Condition = p.parseExpression(LOWEST)

		// Expect 'then'
		if !p.expectPeek(TOKEN_THEN) {
			return nil
		}

		// Move past 'then'
		p.nextToken()

		// Parse consequence block
		elseif.Consequence = p.parseBlockStatement(TOKEN_ELSEIF, TOKEN_ELSE, TOKEN_END)

		expr.Alternatives = append(expr.Alternatives, elseif)
	}

	// Handle else clause
	if p.curTokenIs(TOKEN_ELSE) {
		// Move past 'else'
		p.nextToken()

		// Parse alternative block
		expr.Alternative = p.parseBlockStatement(TOKEN_END)
	}

	// Expect 'end'
	if !p.curTokenIs(TOKEN_END) {
		return nil
	}

	return expr
}

// parseForStatement parses a for loop (numeric or generic)
func (p *Parser) parseForStatement() Statement {
	forToken := p.curToken // 'for'

	// Move to first identifier
	if !p.expectPeek(TOKEN_IDENT) {
		return nil
	}

	firstVar := &Identifier{
		Token: p.curToken,
		Value: p.curToken.Literal,
	}

	// Check what comes after the first identifier
	// If '=', it's a numeric for loop
	// If ',', it's a generic for loop with multiple vars
	// If 'in', it's a generic for loop with one var
	p.nextToken()

	if p.curTokenIs(TOKEN_ASSIGN) {
		// Numeric for loop: for i = start, end, step do ... end
		return p.parseNumericForStatement(forToken, firstVar)
	} else if p.curTokenIs(TOKEN_COMMA) || p.curTokenIs(TOKEN_IN) {
		// Generic for loop: for k, v in expr do ... end
		return p.parseGenericForStatement(forToken, firstVar)
	}

	return nil
}

// parseNumericForStatement parses a numeric for loop
func (p *Parser) parseNumericForStatement(forToken Token, varName *Identifier) *NumericForStatement {
	stmt := &NumericForStatement{
		Token:   forToken,
		VarName: varName,
	}

	// Move past '='
	p.nextToken()

	// Parse start expression
	stmt.Start = p.parseExpression(LOWEST)

	// Expect ','
	if !p.expectPeek(TOKEN_COMMA) {
		return nil
	}

	// Move past ','
	p.nextToken()

	// Parse end expression
	stmt.End = p.parseExpression(LOWEST)

	// Check for optional step
	if p.peekTokenIs(TOKEN_COMMA) {
		p.nextToken() // consume ','
		p.nextToken() // move to step expression
		stmt.Step = p.parseExpression(LOWEST)
	}

	// Expect 'do'
	if !p.expectPeek(TOKEN_DO) {
		return nil
	}

	// Move past 'do'
	p.nextToken()

	// Parse body
	stmt.Body = p.parseBlockStatement(TOKEN_END)

	// Expect 'end'
	if !p.curTokenIs(TOKEN_END) {
		return nil
	}

	return stmt
}

// parseGenericForStatement parses a generic for loop
func (p *Parser) parseGenericForStatement(forToken Token, firstVar *Identifier) *GenericForStatement {
	stmt := &GenericForStatement{
		Token:    forToken,
		VarNames: []*Identifier{firstVar},
	}

	// Collect all variable names
	for p.curTokenIs(TOKEN_COMMA) {
		// Move past ','
		p.nextToken()

		if !p.curTokenIs(TOKEN_IDENT) {
			return nil
		}

		stmt.VarNames = append(stmt.VarNames, &Identifier{
			Token: p.curToken,
			Value: p.curToken.Literal,
		})

		p.nextToken()
	}

	// Expect 'in'
	if !p.curTokenIs(TOKEN_IN) {
		return nil
	}

	// Move past 'in'
	p.nextToken()

	// Parse iterator expression
	stmt.Iterator = p.parseExpression(LOWEST)

	// Expect 'do'
	if !p.expectPeek(TOKEN_DO) {
		return nil
	}

	// Move past 'do'
	p.nextToken()

	// Parse body
	stmt.Body = p.parseBlockStatement(TOKEN_END)

	// Expect 'end'
	if !p.curTokenIs(TOKEN_END) {
		return nil
	}

	return stmt
}

// parseBlockStatement parses a block of statements until one of the terminator tokens
func (p *Parser) parseBlockStatement(terminators ...TokenType) *BlockStatement {
	block := &BlockStatement{
		Token:      p.curToken,
		Statements: []Statement{},
	}

	for !p.curTokenIs(TOKEN_EOF) {
		// Check if we hit a terminator
		isTerminator := false
		for _, term := range terminators {
			if p.curTokenIs(term) {
				isTerminator = true
				break
			}
		}
		if isTerminator {
			break
		}

		stmt := p.parseStatement()
		if stmt != nil {
			block.Statements = append(block.Statements, stmt)
		}

		p.nextToken()
	}

	return block
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
	case TOKEN_IF:
		leftExp = p.parseIfExpression()
	// Keywords that can be used as identifiers in expressions (function names, etc.)
	case TOKEN_MAKE, TOKEN_FREE, TOKEN_FUNCTION, TOKEN_LOCAL,
		TOKEN_THEN, TOKEN_ELSE, TOKEN_ELSEIF, TOKEN_END,
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
