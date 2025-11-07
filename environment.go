package main

import "fmt"

// Variable represents a variable binding with mutability tracking
type Variable struct {
	Value   Object
	Mutable bool
	Line    int // Line where declared
	Column  int // Column where declared
}

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
		// Variable exists in current scope, check if it's mutable
		if !existing.Mutable {
			return fmt.Errorf("cannot reassign immutable variable '%s' (declared at line %d, col %d)",
				name, existing.Line, existing.Column)
		}
		// Update mutable variable
		existing.Value = value
		return nil
	}

	// Check if variable exists in outer scope
	if e.outer != nil {
		if existing, exists := e.outer.Get(name); exists {
			// Variable exists in outer scope, check if it's mutable
			if !existing.Mutable {
				return fmt.Errorf("cannot reassign immutable variable '%s' (declared at line %d, col %d)",
					name, existing.Line, existing.Column)
			}
			// Update the variable value (preserve its mutable status and location)
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

// GetAllVariables returns all variables in the environment (for var report)
func (e *Environment) GetAllVariables() map[string]*Variable {
	return e.store
}
