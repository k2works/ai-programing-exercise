package main

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestCalculator_Add(t *testing.T) {
	calculator := NewCalculator()

	tests := []struct {
		name     string
		a, b     int
		expected int
	}{
		{"正の数の加算", 1, 2, 3},
		{"負の数の加算", -1, -2, -3},
		{"ゼロの加算", 0, 5, 5},
		{"大きな数の加算", 1000, 2000, 3000},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := calculator.Add(tt.a, tt.b)
			assert.Equal(t, tt.expected, result)
		})
	}
}

func TestCalculator_Subtract(t *testing.T) {
	calculator := NewCalculator()

	tests := []struct {
		name     string
		a, b     int
		expected int
	}{
		{"正の数の減算", 5, 3, 2},
		{"負の数の減算", -1, -2, 1},
		{"ゼロの減算", 5, 0, 5},
		{"結果が負になる減算", 3, 5, -2},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := calculator.Subtract(tt.a, tt.b)
			assert.Equal(t, tt.expected, result)
		})
	}
}

func TestCalculator_Multiply(t *testing.T) {
	calculator := NewCalculator()

	tests := []struct {
		name     string
		a, b     int
		expected int
	}{
		{"正の数の乗算", 3, 4, 12},
		{"負の数の乗算", -2, 3, -6},
		{"ゼロの乗算", 0, 5, 0},
		{"1との乗算", 7, 1, 7},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := calculator.Multiply(tt.a, tt.b)
			assert.Equal(t, tt.expected, result)
		})
	}
}

func TestCalculator_Divide(t *testing.T) {
	calculator := NewCalculator()

	tests := []struct {
		name        string
		a, b        int
		expected    float64
		expectError bool
	}{
		{"正の数の除算", 10, 2, 5.0, false},
		{"小数になる除算", 7, 2, 3.5, false},
		{"負の数の除算", -10, 2, -5.0, false},
		{"1との除算", 5, 1, 5.0, false},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result, err := calculator.Divide(tt.a, tt.b)
			if tt.expectError {
				assert.Error(t, err)
			} else {
				assert.NoError(t, err)
				assert.Equal(t, tt.expected, result)
			}
		})
	}
}

func TestCalculator_Divide_ZeroError(t *testing.T) {
	calculator := NewCalculator()

	result, err := calculator.Divide(10, 0)
	assert.Error(t, err)
	assert.Equal(t, "ゼロで割ることはできません", err.Error())
	assert.Equal(t, 0.0, result)
}
