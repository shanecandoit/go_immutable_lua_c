package main

import "fmt"

// Variable represents a variable in the environment.
type Variable struct {
	Value   Object
	Mutable bool
	Line    int
	Column  int
}

// SetVariable sets a variable in the environment.
func SetVariable(env *Environment, name string, value Object, mutable bool, line, column int) error {
	if existingVar, exists := env.store[name]; exists && !existingVar.Mutable {
		return fmt.Errorf("cannot reassign immutable variable '%s'", name)
	}

	env.store[name] = &Variable{
		Value:   value,
		Mutable: mutable,
		Line:    line,
		Column:  column,
	}
	return nil
}

// UpdateLoopVariable updates or creates a loop variable in the environment.
func UpdateLoopVariable(env *Environment, name string, value Object, line, column int) {
	if loopVar, exists := env.store[name]; exists {
		loopVar.Value = value
	} else {
		env.store[name] = &Variable{
			Value:   value,
			Mutable: false, // Loop variables are read-only in Lua
			Line:    line,
			Column:  column,
		}
	}
}
