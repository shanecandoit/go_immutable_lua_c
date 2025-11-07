package main

import (
	"errors"
	"fmt"
	"time"
)

// AccessInfo holds information about a memory access.
type AccessInfo struct {
	Line   int
	Column int
	Time   time.Time
}

// PointerMetadata holds metadata about a memory allocation.
type PointerMetadata struct {
	AllocLine   int
	AllocColumn int
	AllocTime   time.Time
	LastWrite   *AccessInfo
	LastRead    *AccessInfo
}

// Pointer represents a memory allocation with metadata.
type Pointer struct {
	Hash     string
	TypeName string
	Size     int64
	Freed    bool
	Metadata *PointerMetadata
}

// Inspect returns a string representation of the Pointer.
func (p *Pointer) Inspect() string {
	if p.Freed {
		return fmt.Sprintf("sha256:%s (freed)", p.Hash[:16])
	}
	return fmt.Sprintf("sha256:%s", p.Hash[:16])
}

// Type returns the object type of the Pointer.
func (p *Pointer) Type() ObjectType {
	return POINTER_OBJ
}

// MemoryManager manages memory allocations.
type MemoryManager struct {
	allocations map[string]*Pointer
}

// NewMemoryManager creates a new MemoryManager.
func NewMemoryManager() *MemoryManager {
	return &MemoryManager{
		allocations: make(map[string]*Pointer),
	}
}

// Allocate creates a new memory allocation.
func (m *MemoryManager) Allocate(typeName string, size int64) (*Pointer, error) {
	if size <= 0 {
		return nil, errors.New("size must be positive")
	}
	ptr := &Pointer{TypeName: typeName, Size: size, Freed: false}
	m.allocations[typeName] = ptr
	return ptr, nil
}

// Free releases a memory allocation.
func (m *MemoryManager) Free(typeName string) error {
	ptr, exists := m.allocations[typeName]
	if !exists {
		return errors.New("pointer not found")
	}
	if ptr.Freed {
		return errors.New("pointer already freed")
	}
	ptr.Freed = true
	return nil
}

// GetAllocations returns all current allocations.
func (m *MemoryManager) GetAllocations() map[string]*Pointer {
	return m.allocations
}
