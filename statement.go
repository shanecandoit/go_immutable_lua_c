package main

import "fmt"

// evalIfStatement evaluates an if statement
func (ev *Evaluator) evalIfStatement(stmt *IfStatement) Object {
	fmt.Println("Evaluating if condition")
	condition := ev.Eval(stmt.Condition)
	if isError(condition) {
		return condition
	}
	fmt.Printf("Condition evaluated to: %v\n", condition)
	if isTruthy(condition) {
		fmt.Println("Condition is truthy, evaluating consequence block")
		return ev.evalBlockStatement(stmt.Consequence)
	}

	// Check elseif clauses
	for _, elseif := range stmt.Alternatives {
		fmt.Println("Evaluating elseif condition")
		condition := ev.Eval(elseif.Condition)
		if isError(condition) {
			return condition
		}
		fmt.Printf("Elseif condition evaluated to: %v\n", condition)
		if isTruthy(condition) {
			fmt.Println("Elseif condition is truthy, evaluating consequence block")
			return ev.evalBlockStatement(elseif.Consequence)
		}
	}

	if stmt.Alternative != nil {
		fmt.Println("Evaluating else block")
		return ev.evalBlockStatement(stmt.Alternative)
	}

	return &Nil{}
}
