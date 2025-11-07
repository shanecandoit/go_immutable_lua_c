package main

import (
	"testing"
)

func TestEnvironmentGetSet(t *testing.T) {
	env := NewEnvironment()

	// Test setting and getting a variable
	err := env.Set("x", &Integer{Value: 10}, true, 1, 1)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	varX, ok := env.Get("x")
	if !ok {
		t.Fatalf("variable 'x' not found")
	}

	if intVal, ok := varX.Value.(*Integer); !ok || intVal.Value != 10 {
		t.Fatalf("expected 10, got %v", varX.Value)
	}

	// Test updating a mutable variable
	err = env.Set("x", &Integer{Value: 20}, true, 1, 1)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	varX, _ = env.Get("x")
	if intVal, ok := varX.Value.(*Integer); !ok || intVal.Value != 20 {
		t.Fatalf("expected 20, got %v", varX.Value)
	}

	// Test immutability
	err = env.Set("y", &Integer{Value: 30}, false, 1, 1)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	err = env.Set("y", &Integer{Value: 40}, false, 1, 1)
	if err == nil {
		t.Fatalf("expected error when updating immutable variable")
	}
}

func TestNestedEnvironment(t *testing.T) {
	outer := NewEnvironment()
	outer.Set("x", &Integer{Value: 10}, true, 1, 1)

	inner := NewEnclosedEnvironment(outer)
	inner.Set("y", &Integer{Value: 20}, true, 1, 1)

	// Test variable retrieval from outer environment
	varX, ok := inner.Get("x")
	if !ok {
		t.Fatalf("variable 'x' not found in outer environment")
	}

	if intVal, ok := varX.Value.(*Integer); !ok || intVal.Value != 10 {
		t.Fatalf("expected 10, got %v", varX.Value)
	}

	// Test variable retrieval from inner environment
	varY, ok := inner.Get("y")
	if !ok {
		t.Fatalf("variable 'y' not found in inner environment")
	}

	if intVal, ok := varY.Value.(*Integer); !ok || intVal.Value != 20 {
		t.Fatalf("expected 20, got %v", varY.Value)
	}

	// Test updating outer environment variable from inner environment
	err := inner.Set("x", &Integer{Value: 30}, true, 1, 1)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	varX, _ = outer.Get("x")
	if intVal, ok := varX.Value.(*Integer); !ok || intVal.Value != 30 {
		t.Fatalf("expected 30, got %v", varX.Value)
	}
}
