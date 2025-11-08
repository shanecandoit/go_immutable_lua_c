package main

import "fmt"

// Environment stores variable bindings
type Environment struct {
	store map[string]*Variable
	outer *Environment
}

// NewEnvironment creates a new environment
func NewEnvironment() *Environment {
	s := make(map[string]*Variable)
	return &Environment{store: s, outer: nil}
}

// NewEnclosedEnvironment creates a new environment with an outer environment
func NewEnclosedEnvironment(outer *Environment) *Environment {
	env := NewEnvironment()
	env.outer = outer
	return env
}

// Get retrieves a variable from the environment
func (e *Environment) Get(name string) (*Variable, bool) {
	obj, ok := e.store[name]
	if !ok && e.outer != nil {
		obj, ok = e.outer.Get(name)
	}
	return obj, ok
}

// Set sets a variable in the environment
func (e *Environment) Set(name string, value Object, mutable bool, line, column int) error {
	// Check if variable already exists in current scope
	if existing, exists := e.store[name]; exists {
		if !existing.Mutable {
			return fmt.Errorf("cannot reassign immutable variable '%s' (declared at line %d, col %d)",
				name, existing.Line, existing.Column)
		}
		existing.Value = value
		return nil
	}

	// Check outer environments for existing variable
	for env := e.outer; env != nil; env = env.outer {
		if existing, exists := env.store[name]; exists {
			if !existing.Mutable {
				return fmt.Errorf("cannot reassign immutable variable '%s' (declared at line %d, col %d)",
					name, existing.Line, existing.Column)
			}
			existing.Value = value
			return nil
		}
	}

	// Create new variable binding in current scope
	e.store[name] = &Variable{
		Value:   value,
		Mutable: mutable,
		Line:    line,
		Column:  column,
	}
	return nil
}

// Update updates a variable in the environment, bypassing immutability checks.
// This is used by the interpreter for loop variables.
func (e *Environment) Update(name string, value Object) {
	if existing, exists := e.store[name]; exists {
		existing.Value = value
	}
}

// GetAllVariables returns all variables in the environment (for var report)
func (e *Environment) GetAllVariables() map[string]*Variable {
	return e.store
}
