package main

// evalIfExpression evaluates an if expression
func (ev *Evaluator) evalIfExpression(expr *IfExpression) Object {
	condition := ev.Eval(expr.Condition)
	if isError(condition) {
		return condition
	}

	if isTruthy(condition) {
		return ev.evalBlockStatement(expr.Consequence)
	}

	// Check elseif clauses
	for _, elseif := range expr.Alternatives {
		condition := ev.Eval(elseif.Condition)
		if isError(condition) {
			return condition
		}
		if isTruthy(condition) {
			return ev.evalBlockStatement(elseif.Consequence)
		}
	}

	if expr.Alternative != nil {
		return ev.evalBlockStatement(expr.Alternative)
	}

	return &Nil{}
}
