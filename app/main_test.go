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

// テスト関数：5を渡したら文字列"Buzz"を返す
func Test5を渡したらBuzzを返す(t *testing.T) {
	assertGenerate(t, 5, "Buzz")
}

// テスト関数：15を渡したら文字列"FizzBuzz"を返す
func Test15を渡したらFizzBuzzを返す(t *testing.T) {
	assertGenerate(t, 15, "FizzBuzz")
}

// 追加のテストケース
func Test6を渡したらFizzを返す(t *testing.T) {
	assertGenerate(t, 6, "Fizz")
}

func Test10を渡したらBuzzを返す(t *testing.T) {
	assertGenerate(t, 10, "Buzz")
}

func Test30を渡したらFizzBuzzを返す(t *testing.T) {
	assertGenerate(t, 30, "FizzBuzz")
}

// テスト関数：1から100までのFizzBuzzを返す
func Test1から100までのFizzBuzzを返す(t *testing.T) {
	results := GenerateList(1, 100)

	// いくつかのキーポイントをテスト
	if results[0] != "1" {
		t.Errorf("results[0] = %v, want 1", results[0])
	}
	if results[2] != "Fizz" { // 3番目の要素（3）
		t.Errorf("results[2] = %v, want Fizz", results[2])
	}
	if results[4] != "Buzz" { // 5番目の要素（5）
		t.Errorf("results[4] = %v, want Buzz", results[4])
	}
	if results[14] != "FizzBuzz" { // 15番目の要素（15）
		t.Errorf("results[14] = %v, want FizzBuzz", results[14])
	}
	if len(results) != 100 {
		t.Errorf("len(results) = %v, want 100", len(results))
	}
}
