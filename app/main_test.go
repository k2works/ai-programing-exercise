package main

import "testing"

// テストヘルパー関数
func assertGenerate(t *testing.T, input int, expected string) {
	t.Helper()
	got := Generate(input)
	if got != expected {
		t.Errorf("Generate(%d) = %v, want %v", input, got, expected)
	}
}

// テスト関数：1を渡したら文字列"1"を返す
func Test1を渡したら文字列1を返す(t *testing.T) {
	assertGenerate(t, 1, "1")
}

// テスト関数：2を渡したら文字列"2"を返す
func Test2を渡したら文字列2を返す(t *testing.T) {
	assertGenerate(t, 2, "2")
}

// テスト関数：3を渡したら文字列"Fizz"を返す
func Test3を渡したらFizzを返す(t *testing.T) {
	assertGenerate(t, 3, "Fizz")
}
