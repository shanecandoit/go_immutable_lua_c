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

// IfExpression represents an if-then-else used as an expression
type IfExpression struct {
	Token        Token
	Condition    Expression
	Consequence  *BlockStatement
	Alternatives []*ElseIfClause
	Alternative  *BlockStatement
}

func (ie *IfExpression) expressionNode()      {}
func (ie *IfExpression) TokenLiteral() string { return ie.Token.Literal }
func (ie *IfExpression) String() string {
	var out string
	out += "if " + ie.Condition.String() + " then\n"
	out += ie.Consequence.String()
	for _, alt := range ie.Alternatives {
		out += alt.String()
	}
	if ie.Alternative != nil {
		out += "else\n" + ie.Alternative.String()
	}
	out += "end"
	return out
}

// IfStatement represents an if-then-else statement
// Lua syntax: if condition then ... elseif condition then ... else ... end
type IfStatement struct {
	Token        Token           // The 'if' token
	Condition    Expression      // The condition expression
	Consequence  *BlockStatement // The 'then' block
	Alternatives []*ElseIfClause // Optional elseif clauses
	Alternative  *BlockStatement // Optional else block
}

func (is *IfStatement) statementNode()       {}
func (is *IfStatement) TokenLiteral() string { return is.Token.Literal }
func (is *IfStatement) String() string {
	var out string
	out += "if " + is.Condition.String() + " then\n"
	out += is.Consequence.String()
	for _, alt := range is.Alternatives {
		out += alt.String()
	}
	if is.Alternative != nil {
		out += "else\n"
		out += is.Alternative.String()
	}
	out += "end"
	return out
}

// ElseIfClause represents an elseif clause
type ElseIfClause struct {
	Token       Token           // The 'elseif' token
	Condition   Expression      // The condition expression
	Consequence *BlockStatement // The block to execute
}

func (eic *ElseIfClause) String() string {
	return "elseif " + eic.Condition.String() + " then\n" + eic.Consequence.String()
}

// BlockStatement represents a block of statements
type BlockStatement struct {
	Token      Token // The first token of the block
	Statements []Statement
}

// String for BlockStatement
func (b *BlockStatement) String() string {
	var out string
	for _, stmt := range b.Statements {
		out += stmt.String()
	}
	return out
}

// Ensure BlockStatement implements Node
func (b *BlockStatement) TokenLiteral() string {
	return b.Token.Literal
}

// Identifier represents a variable name or identifier
type Identifier struct {
	Token Token
	Value string
}

// String returns the string representation of the identifier
func (i *Identifier) String() string {
	return i.Value
}

// TokenLiteral returns the literal value of the token
func (i *Identifier) TokenLiteral() string {
	return i.Token.Literal
}

// Ensure Identifier fully implements Expression
func (i *Identifier) expressionNode() {}

// NumericForStatement represents a numeric for loop
type NumericForStatement struct {
	Token   Token
	VarName *Identifier
	Start   Expression
	End     Expression
	Step    Expression
	Body    *BlockStatement
}

// TokenLiteral for NumericForStatement
func (n *NumericForStatement) TokenLiteral() string {
	return n.Token.Literal
}

// String for NumericForStatement
func (n *NumericForStatement) String() string {
	return "for " + n.VarName.String() + " = " + n.Start.String() + ", " + n.End.String()
}

// Add statementNode method for NumericForStatement
func (n *NumericForStatement) statementNode() {}

// GenericForStatement represents a generic for loop
type GenericForStatement struct {
	Token    Token
	VarNames []*Identifier
	Iterator Expression
	Body     *BlockStatement
}

// TokenLiteral for GenericForStatement
func (g *GenericForStatement) TokenLiteral() string {
	return g.Token.Literal
}

// String for GenericForStatement
func (g *GenericForStatement) String() string {
	return "for ... in ..." // Simplified for now
}

// Add statementNode method for GenericForStatement
func (g *GenericForStatement) statementNode() {}

// IntegerLiteral represents an integer literal
type IntegerLiteral struct {
	Token Token
	Value string
}

// TokenLiteral for IntegerLiteral
func (i *IntegerLiteral) TokenLiteral() string {
	return i.Token.Literal
}

// String for IntegerLiteral
func (i *IntegerLiteral) String() string {
	return i.Value
}

// Add expressionNode method for IntegerLiteral
func (i *IntegerLiteral) expressionNode() {}

// StringLiteral represents a string literal
type StringLiteral struct {
	Token Token
	Value string
}

// TokenLiteral for StringLiteral
func (s *StringLiteral) TokenLiteral() string {
	return s.Token.Literal
}

// String for StringLiteral
func (s *StringLiteral) String() string {
	return s.Value
}

// Add expressionNode method for StringLiteral
func (s *StringLiteral) expressionNode() {}

// BooleanLiteral represents a boolean literal
type BooleanLiteral struct {
	Token Token
	Value bool
}

// TokenLiteral for BooleanLiteral
func (b *BooleanLiteral) TokenLiteral() string {
	return b.Token.Literal
}

// String for BooleanLiteral
func (b *BooleanLiteral) String() string {
	return fmt.Sprintf("%t", b.Value)
}

// Add expressionNode method for BooleanLiteral
func (b *BooleanLiteral) expressionNode() {}

// NilLiteral represents a nil literal
type NilLiteral struct {
	Token Token
}

// TokenLiteral for NilLiteral
func (n *NilLiteral) TokenLiteral() string {
	return n.Token.Literal
}

// String for NilLiteral
func (n *NilLiteral) String() string {
	return "nil"
}

// Add expressionNode method for NilLiteral
func (n *NilLiteral) expressionNode() {}

// PrefixExpression represents a prefix expression
type PrefixExpression struct {
	Token    Token
	Operator string
	Right    Expression
}

// TokenLiteral for PrefixExpression
func (p *PrefixExpression) TokenLiteral() string {
	return p.Token.Literal
}

// String for PrefixExpression
func (p *PrefixExpression) String() string {
	return "(" + p.Operator + p.Right.String() + ")"
}

// Add expressionNode method for PrefixExpression
func (p *PrefixExpression) expressionNode() {}

// InfixExpression represents an infix expression
type InfixExpression struct {
	Token    Token
	Left     Expression
	Operator string
	Right    Expression
}

// TokenLiteral for InfixExpression
func (i *InfixExpression) TokenLiteral() string {
	return i.Token.Literal
}

// String for InfixExpression
func (i *InfixExpression) String() string {
	return "(" + i.Left.String() + " " + i.Operator + " " + i.Right.String() + ")"
}

// Add expressionNode method for InfixExpression
func (i *InfixExpression) expressionNode() {}

// CallExpression represents a function call
type CallExpression struct {
	Token     Token
	Function  Expression
	Arguments []Expression
}

// TokenLiteral for CallExpression
func (c *CallExpression) TokenLiteral() string {
	return c.Token.Literal
}

// String for CallExpression
func (c *CallExpression) String() string {
	return c.Function.String() + "(...)"
}

// Add expressionNode method for CallExpression
func (c *CallExpression) expressionNode() {}

// FloatLiteral represents a floating-point number
// Ensure FloatLiteral implements Expression
type FloatLiteral struct {
	Token Token
	Value string
}

func (fl *FloatLiteral) expressionNode() {}
func (fl *FloatLiteral) TokenLiteral() string {
	return fl.Token.Literal
}
func (fl *FloatLiteral) String() string {
	return fl.Value
}
