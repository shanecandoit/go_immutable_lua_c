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
	fmt.Printf("Getting variable '%s'\n", name)
	obj, ok := e.store[name]
	// Add debug statement to trace variable retrieval in nested environments
	fmt.Printf("Searching for variable '%s' in current environment\n", name)
	if !ok && e.outer != nil {
		fmt.Printf("Variable '%s' not found in current environment. Checking outer environment.\n", name)
		obj, ok = e.outer.Get(name)
	}
	return obj, ok
}

// Set sets a variable in the environment
func (e *Environment) Set(name string, value Object, mutable bool, line, column int) error {
	fmt.Printf("Setting variable '%s' to value: %v\n", name, value)

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
			fmt.Printf("Updated variable '%s' in outer environment to value: %v\n", name, value)
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
	fmt.Printf("Created new variable '%s' in current environment with value: %v\n", name, value)
	return nil
}

// GetAllVariables returns all variables in the environment (for var report)
func (e *Environment) GetAllVariables() map[string]*Variable {
	return e.store
}
