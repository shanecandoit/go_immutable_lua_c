package main

// evalIfStatement evaluates an if statement
func (ev *Evaluator) evalIfStatement(stmt *IfStatement) Object {
	condition := ev.Eval(stmt.Condition)
	if isError(condition) {
		return condition
	}

	if isTruthy(condition) {
		return ev.evalScopedBlockStatement(stmt.Consequence)
	}

	// Check elseif clauses
	for _, elseif := range stmt.Alternatives {
		condition := ev.Eval(elseif.Condition)
		if isError(condition) {
			return condition
		}
		if isTruthy(condition) {
			return ev.evalScopedBlockStatement(elseif.Consequence)
		}
	}

	if stmt.Alternative != nil {
		return ev.evalScopedBlockStatement(stmt.Alternative)
	}

	return &Nil{}
}

// evalScopedBlockStatement evaluates a block statement in a new scope
func (ev *Evaluator) evalScopedBlockStatement(block *BlockStatement) Object {
	env := NewEnclosedEnvironment(ev.env)
	ev.env = env
	defer func() {
		ev.env = ev.env.outer
	}()

	return ev.evalBlockStatement(block)
}
