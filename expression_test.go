package main

import (
	"testing"
)

func TestEvalIntegerExpression(t *testing.T) {
	tests := []struct {
		input    string
		expected int64
	}{
		{"x = 5", 5},
		{"x = 10", 10},
		{"x = -5", -5},
		{"x = 5 + 5", 10},
		{"x = 5 - 3", 2},
		{"x = 5 * 2", 10},
		{"x = 10 / 2", 5},
		{"x = 10 % 3", 1},
		{"x = 2 + 3 * 4", 14},
		{"x = (2 + 3) * 4", 20},
	}

	for _, tt := range tests {
		evaluated := testEval(tt.input)
		testIntegerObject(t, evaluated, tt.expected)
	}
}

func TestEvalBooleanExpression(t *testing.T) {
	tests := []struct {
		input    string
		expected bool
	}{
		{"x = true", true},
		{"x = false", false},
		{"x = 1 < 2", true},
		{"x = 1 > 2", false},
		{"x = 1 == 1", true},
		{"x = 1 != 2", true},
	}

	for _, tt := range tests {
		evaluated := testEval(tt.input)
		testBooleanObject(t, evaluated, tt.expected)
	}
}
