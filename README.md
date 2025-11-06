# Immutable Lua/C Scripting Language

A memory-safe scripting language that combines Lua and C syntax with immutability-by-default semantics, implemented in Go. Variables are immutable unless explicitly marked with `mut`, and all pointers are represented as SHA256 hashes for enhanced security and debugging.

## Features

### Immutability by Default

- Variables are **immutable by default**
- Use `mut` keyword to declare mutable variables
- Encourages functional programming patterns and safer code

### SHA256 Pointer System

- Pointers are represented as SHA256 hashes instead of memory addresses
- Enhanced security and deterministic behavior
- Easier debugging with human-readable hash identifiers

### Memory Management with Reports

- `make` keyword for allocating memory
- `free` keyword for explicit deallocation (optional)
- **Free Report**: Generated after script execution, similar to Valgrind
  - Lists all variables that were not freed
  - Shows last read and write operations for each leaked variable
  - Encourages proper memory management practices

### Variable State Export

- **Var Report**: JSON export of all top-level variables and their values
- Useful for debugging, testing, and state inspection
- Generated automatically on script execution

## Usage

### Running a Script

```bash
./go_immutable_lua_c script.lua
```

This will:

1. Execute the script
2. Generate a **Free Report** showing any memory leaks
3. Generate a **Var Report** (JSON file) with top-level variable states

### Basic Syntax Examples

#### Immutable Variables (Default)

```lua
-- Immutable variable declaration
x = 10
name = "Alice"

-- This would cause an error:
-- x = 20  -- Error: cannot reassign immutable variable
```

#### Mutable Variables

```lua
-- Declare a mutable variable
mut counter = 0
counter = counter + 1  -- OK
counter = counter + 1  -- OK
```

#### Pointers with SHA256 Hashes

```lua
-- Allocate memory and get SHA256 hash pointer
mut ptr = make(int, 100)
-- ptr is now a SHA256 hash like "a3f5d8c2e1b4..."

-- Use the pointer
*ptr = 42

-- Free memory (optional but encouraged)
free(ptr)
```

#### C-style Pointer Operations

```c
// Allocate an array
mut arr = make(int, 10);

// Access elements
arr[0] = 5;
arr[1] = 10;

// Free when done
free(arr);
```

## Memory Management Reports

### Free Report

After script execution, a free report is generated showing memory leaks:

```
=== Memory Free Report ===
Leaked Variables: 2

Variable: data
  Type: *int
  Pointer: sha256:a3f5d8c2e1b4f9a7c8d3e2f1a5b9c4d7e3f8a2b5c9d4e1f7a3b8c5d2e9f4a1b6
  Last Write: line 15, column 5
  Last Read: line 23, column 12
  Status: NOT FREED

Variable: buffer
  Type: *[]byte
  Pointer: sha256:b7c4d1e8f5a2b9c6d3e0f7a4b1c8d5e2f9a6b3c0d7e4f1a8b5c2d9e6f3a0b7
  Last Write: line 8, column 3
  Last Read: line 18, column 7
  Status: NOT FREED

Total Allocated: 12
Total Freed: 10
Memory Efficiency: 83.33%
```

### Var Report (JSON)

The var report exports all top-level variables as JSON:

```json
{
  "timestamp": "2025-11-05T10:30:45Z",
  "script": "example.lua",
  "variables": {
    "counter": {
      "type": "int",
      "value": 42,
      "mutable": true
    },
    "name": {
      "type": "string",
      "value": "Alice",
      "mutable": false
    },
    "data": {
      "type": "*int",
      "value": "sha256:a3f5d8c2e1b4f9a7c8d3e2f1a5b9c4d7e3f8a2b5c9d4e1f7a3b8c5d2e9f4a1b6",
      "mutable": true,
      "freed": false
    }
  }
}
```

## Language Reference

### Keywords

- `mut` - Declare a mutable variable
- `make` - Allocate memory and return SHA256 pointer
- `free` - Deallocate memory (optional)
- `*` - Dereference pointer
- `&` - Address-of operator

### Type System

Supports both Lua and C type conventions:

- **Primitive types**: `int`, `float`, `string`, `bool`
- **Pointer types**: `*int`, `*float`, etc.
- **Composite types**: arrays, tables, structs

### Memory Safety

The interpreter tracks:

- Allocation site (line/column)
- Last write operation
- Last read operation
- Free status

## Examples

### Example 1: Basic Immutability

```lua
-- Immutable by default
config = {
  host = "localhost",
  port = 8080
}

-- config.port = 9090  -- Error: cannot modify immutable variable

-- Use mut for mutable data
mut state = {
  connected = false,
  attempts = 0
}

state.connected = true  -- OK
state.attempts = 1      -- OK
```

### Example 2: Memory Management

```lua
function processData()
  -- Allocate memory
  mut buffer = make(byte, 1024)
  
  -- Use the buffer
  -- ... operations ...
  
  -- Clean up
  free(buffer)
end

-- Without free, the buffer would appear in the Free Report
```

### Example 3: Pointer Arithmetic with SHA256

```lua
mut arr = make(int, 5)

-- SHA256 ensures deterministic behavior
print(arr)  -- sha256:a3f5d8c2e1b4f9a7...

-- Access via dereference
*arr = 10

free(arr)
```

## Benefits

1. **Memory Safety**: Catch memory leaks before production
2. **Functional Style**: Immutability encourages cleaner code
3. **Debugging**: SHA256 pointers provide consistent, trackable references
4. **Transparency**: Full visibility into variable state and memory usage
5. **Educational**: Learn proper memory management with helpful reports

## License

AGPL-3.0 License

## Roadmap

- [ ] Support for custom allocators
- [ ] Interactive debugger with memory visualization
- [ ] Performance profiling integration
- [ ] Foreign function interface (FFI) for C libraries
- [ ] Garbage collection mode as alternative to manual free
- [ ] WASM compilation target

## Acknowledgments

Inspired by:

- Lua's simplicity and elegance
- C's explicit memory management
- Rust's ownership and borrowing concepts
- Valgrind's memory leak detection
