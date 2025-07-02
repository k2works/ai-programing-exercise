package main

import "errors"

// Calculator は基本的な四則演算を提供する構造体
type Calculator struct{}

// NewCalculator は新しいCalculatorインスタンスを作成する
func NewCalculator() *Calculator {
	return &Calculator{}
}

// Add は2つの数値を加算する
func (c *Calculator) Add(a, b int) int {
	return a + b
}

// Subtract は2つの数値を減算する
func (c *Calculator) Subtract(a, b int) int {
	return a - b
}

// Multiply は2つの数値を乗算する
func (c *Calculator) Multiply(a, b int) int {
	return a * b
}

// Divide は2つの数値を除算する（ゼロ除算エラーハンドリング付き）
func (c *Calculator) Divide(a, b int) (float64, error) {
	if b == 0 {
		return 0, errors.New("ゼロで割ることはできません")
	}
	return float64(a) / float64(b), nil
}
