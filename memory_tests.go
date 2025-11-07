package main

import (
	"testing"
)

func TestAllocateMemory(t *testing.T) {
	manager := NewMemoryManager()
	ptr, err := manager.Allocate("int", 100)
	if err != nil {
		t.Fatalf("unexpected error: %s", err)
	}
	if ptr.TypeName != "int" || ptr.Size != 100 || ptr.Freed {
		t.Errorf("allocation failed: %+v", ptr)
	}
}

func TestFreeMemory(t *testing.T) {
	manager := NewMemoryManager()
	_, err := manager.Allocate("int", 100)
	if err != nil {
		t.Fatalf("unexpected error: %s", err)
	}
	err = manager.Free("int")
	if err != nil {
		t.Fatalf("unexpected error: %s", err)
	}

	err = manager.Free("int")
	if err == nil || err.Error() != "pointer already freed" {
		t.Errorf("expected double free error, got: %v", err)
	}
}

func TestInvalidAllocation(t *testing.T) {
	manager := NewMemoryManager()
	_, err := manager.Allocate("int", -10)
	if err == nil || err.Error() != "size must be positive" {
		t.Errorf("expected size error, got: %v", err)
	}
}

func TestFreeNonExistentPointer(t *testing.T) {
	manager := NewMemoryManager()
	err := manager.Free("nonexistent")
	if err == nil || err.Error() != "pointer not found" {
		t.Errorf("expected pointer not found error, got: %v", err)
	}
}
