package main

import (
	"fmt"
	"reflect"
)

// Parser represents the parser
type Parser struct {
	lexer          *Lexer
	curToken       Token
	peekToken      Token
	errors         []string
	prefixParseFns map[TokenType]prefixParseFn
	infixParseFns  map[TokenType]infixParseFn
}

type (
	prefixParseFn func() Expression
	infixParseFn  func(Expression) Expression
)

// NewParser creates a new parser
func NewParser(l *Lexer) *Parser {
	p := &Parser{
		lexer:  l,
		errors: []string{},
	}

	p.prefixParseFns = make(map[TokenType]prefixParseFn)
	p.registerPrefix(TOKEN_IDENT, p.parseIdentifier)
	p.registerPrefix(TOKEN_NUMBER, p.parseNumberLiteral)
	p.registerPrefix(TOKEN_STRING, p.parseStringLiteral)
	p.registerPrefix(TOKEN_MAKE, p.parseIdentifier)
	p.registerPrefix(TOKEN_FREE, p.parseIdentifier)
	p.registerPrefix(TOKEN_TRUE, p.parseBoolean)
	p.registerPrefix(TOKEN_FALSE, p.parseBoolean)
	p.registerPrefix(TOKEN_NIL, p.parseNil)
	p.registerPrefix(TOKEN_MINUS, p.parsePrefixExpression)
	p.registerPrefix(TOKEN_NOT, p.parsePrefixExpression)
	p.registerPrefix(TOKEN_STAR, p.parsePrefixExpression)
	p.registerPrefix(TOKEN_AMPERSAND, p.parsePrefixExpression)
	p.registerPrefix(TOKEN_HASH, p.parsePrefixExpression)
	p.registerPrefix(TOKEN_LPAREN, p.parseGroupedExpression)
	p.registerPrefix(TOKEN_IF, p.parseIfExpression)
	p.registerPrefix(TOKEN_FUNCTION, p.parseFunctionLiteral)

	p.infixParseFns = make(map[TokenType]infixParseFn)
	p.registerInfix(TOKEN_PLUS, p.parseInfixExpression)
	p.registerInfix(TOKEN_MINUS, p.parseInfixExpression)
	p.registerInfix(TOKEN_SLASH, p.parseInfixExpression)
	p.registerInfix(TOKEN_STAR, p.parseInfixExpression)
	p.registerInfix(TOKEN_PERCENT, p.parseInfixExpression)
	p.registerInfix(TOKEN_EQ, p.parseInfixExpression)
	p.registerInfix(TOKEN_NE, p.parseInfixExpression)
	p.registerInfix(TOKEN_LT, p.parseInfixExpression)
	p.registerInfix(TOKEN_LE, p.parseInfixExpression)
	p.registerInfix(TOKEN_GT, p.parseInfixExpression)
	p.registerInfix(TOKEN_GE, p.parseInfixExpression)
	p.registerInfix(TOKEN_AND, p.parseInfixExpression)
	p.registerInfix(TOKEN_OR, p.parseInfixExpression)
	p.registerInfix(TOKEN_CONCAT, p.parseInfixExpression)
	p.registerInfix(TOKEN_CARET, p.parseInfixExpression)
	p.registerInfix(TOKEN_LPAREN, p.parseCallExpression)

	// Read two tokens, so curToken and peekToken are both set
	p.nextToken()
	p.nextToken()

	return p
}

func (p *Parser) registerPrefix(tokenType TokenType, fn prefixParseFn) {
	p.prefixParseFns[tokenType] = fn
}

func (p *Parser) registerInfix(tokenType TokenType, fn infixParseFn) {
	p.infixParseFns[tokenType] = fn
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
			// Avoid appending typed-nil values (e.g., (*X)(nil) stored in an interface)
			rv := reflect.ValueOf(stmt)
			if rv.Kind() == reflect.Ptr && rv.IsNil() {
				// skip
			} else {
				program.Statements = append(program.Statements, stmt)
			}
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
	case TOKEN_RETURN:
		return p.parseReturnStatement()
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

	expr := p.parseExpression(LOWEST)
	if expr == nil {
		return nil
	}

	stmt.Expression = expr
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
	val := p.parseExpression(LOWEST)
	if val == nil {
		return nil
	}
	stmt.Value = val

	return stmt
}

func (p *Parser) parseReturnStatement() *ReturnStatement {
	stmt := &ReturnStatement{Token: p.curToken}

	p.nextToken()

	stmt.ReturnValue = p.parseExpression(LOWEST)

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

func (p *Parser) parseExpression(precedence int) Expression {
	prefix := p.prefixParseFns[p.curToken.Type]
	if prefix == nil {
		p.noPrefixParseFnError(p.curToken.Type)
		return nil
	}
	leftExp := prefix()

	for !p.peekTokenIs(TOKEN_EOF) && precedence < p.peekPrecedence() {
		infix := p.infixParseFns[p.peekToken.Type]
		if infix == nil {
			return leftExp
		}
		p.nextToken()
		leftExp = infix(leftExp)
	}

	return leftExp
}

func (p *Parser) parseIdentifier() Expression {
	return &Identifier{Token: p.curToken, Value: p.curToken.Literal}
}

func (p *Parser) parseStringLiteral() Expression {
	return &StringLiteral{Token: p.curToken, Value: p.curToken.Literal}
}

func (p *Parser) parseBoolean() Expression {
	return &BooleanLiteral{Token: p.curToken, Value: p.curTokenIs(TOKEN_TRUE)}
}

func (p *Parser) parseNil() Expression {
	return &NilLiteral{Token: p.curToken}
}

func (p *Parser) parseGroupedExpression() Expression {
	p.nextToken()
	exp := p.parseExpression(LOWEST)
	if !p.expectPeek(TOKEN_RPAREN) {
		return nil
	}
	return exp
}

func (p *Parser) parseFunctionLiteral() Expression {
	lit := &FunctionLiteral{Token: p.curToken}

	if !p.expectPeek(TOKEN_LPAREN) {
		return nil
	}

	lit.Parameters = p.parseFunctionParameters()

	// after parseFunctionParameters, curToken is ')'
	p.nextToken()

	lit.Body = p.parseBlockStatement(TOKEN_END)

	return lit
}

func (p *Parser) parseFunctionParameters() []*Identifier {
	identifiers := []*Identifier{}

	if p.peekTokenIs(TOKEN_RPAREN) {
		p.nextToken()
		return identifiers
	}

	p.nextToken()

	ident := &Identifier{Token: p.curToken, Value: p.curToken.Literal}
	identifiers = append(identifiers, ident)

	for p.peekTokenIs(TOKEN_COMMA) {
		p.nextToken()
		p.nextToken()
		ident := &Identifier{Token: p.curToken, Value: p.curToken.Literal}
		identifiers = append(identifiers, ident)
	}

	if !p.expectPeek(TOKEN_RPAREN) {
		return nil
	}

	return identifiers
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
