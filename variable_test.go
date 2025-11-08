package main

import "testing"

func TestSetVariable(t *testing.T) {
	env := NewEnvironment()
	err := SetVariable(env, "x", &Integer{Value: 42}, true, 1, 1)
	if err != nil {
		t.Fatalf("unexpected error: %s", err)
	}

	varObj, exists := env.store["x"]
	if !exists {
		t.Fatalf("variable 'x' not found in environment")
	}

	if varObj.Value.Inspect() != "42" {
		t.Errorf("expected value 42, got %s", varObj.Value.Inspect())
	}

	if !varObj.Mutable {
		t.Errorf("expected variable to be mutable")
	}
}

func TestUpdateLoopVariable(t *testing.T) {
	env := NewEnvironment()
	UpdateLoopVariable(env, "i", &Integer{Value: 1}, 1, 1)

	varObj, exists := env.store["i"]
	if !exists {
		t.Fatalf("loop variable 'i' not found in environment")
	}

	if varObj.Value.Inspect() != "1" {
		t.Errorf("expected value 1, got %s", varObj.Value.Inspect())
	}

	if varObj.Mutable {
		t.Errorf("expected loop variable to be immutable")
	}
}
