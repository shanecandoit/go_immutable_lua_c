package main

import "fmt"

func main() {
	fmt.Println("# Immutable Lua/C Interpreter")
	fmt.Println()

	// Example: Evaluate some code
	input := `
	x = 10
	mut y = 20
	z = x + y
	y = y * 2
	result = z + y
	name = "Hello" .. " " .. "World"
	`

	lexer := NewLexer(input)
	parser := NewParser(lexer)
	program := parser.ParseProgram()

	if len(parser.Errors()) > 0 {
		fmt.Println("Parser errors:\n")
		for _, err := range parser.Errors() {
			fmt.Println("- ", err)
		}
		return
	}

	fmt.Println("Evaluating code...")

	evaluator := NewEvaluator()
	result := evaluator.Eval(program)

	if result != nil && result.Type() == ERROR_OBJ {
		fmt.Println("Evaluation error:")
		fmt.Println("  ", result.Inspect())
		return
	}

	fmt.Println("Execution successful!")
	fmt.Println()
	fmt.Println("Variables:")
	fmt.Println()

	vars := evaluator.GetEnvironment().GetAllVariables()
	for name, variable := range vars {
		mutStr := "immutable"
		if variable.Mutable {
			mutStr = "mutable"
		}
		fmt.Printf("-  %s = %s (%s)\n", name, variable.Value.Inspect(), mutStr)
	}

	fmt.Println()
	fmt.Println("Try modifying an immutable variable:")
	fmt.Println("-------------------------------------")

	// Demonstrate immutability error
	badInput := `
	x = 10
	x = 20
	`

	lexer2 := NewLexer(badInput)
	parser2 := NewParser(lexer2)
	program2 := parser2.ParseProgram()
	evaluator2 := NewEvaluator()
	result2 := evaluator2.Eval(program2)

	if result2 != nil && result2.Type() == ERROR_OBJ {
		fmt.Println("  " + result2.Inspect())
	}

	fmt.Println()
}
