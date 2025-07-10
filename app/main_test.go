package main

import "testing"

func assertGenerateByType(t *testing.T, number, fizzBuzzType int, expected string) {
	t.Helper()
	got := GenerateByType(number, fizzBuzzType)
	if got != expected {
		t.Errorf("GenerateByType(%d, %d) = %v, want %v", number, fizzBuzzType, got, expected)
	}
}

// 数を文字列にして返す
// タイプ1の場合

// 三の倍数の場合
func Test3を渡したら文字列Fizzを返す_タイプ1(t *testing.T) {
	assertGenerateByType(t, 3, 1, "Fizz")
}

// 五の倍数の場合
func Test5を渡したら文字列Buzzを返す_タイプ1(t *testing.T) {
	assertGenerateByType(t, 5, 1, "Buzz")
}

// 三と五の倍数の場合
func Test15を渡したら文字列FizzBuzzを返す_タイプ1(t *testing.T) {
	assertGenerateByType(t, 15, 1, "FizzBuzz")
}

// その他の場合
func Test1を渡したら文字列1を返す_タイプ1(t *testing.T) {
	assertGenerateByType(t, 1, 1, "1")
}

// 1から100までのFizzBuzzの配列を返す
func Test1から100までのFizzBuzzを返す_タイプ1(t *testing.T) {
	results := GenerateList(1, 100)

	// いくつかの値を確認
	if len(results) != 100 {
		t.Errorf("Length = %d, want 100", len(results))
	}

	// 具体的な値を確認
	if results[0] != "1" {
		t.Errorf("results[0] = %s, want 1", results[0])
	}
	if results[2] != "Fizz" {
		t.Errorf("results[2] = %s, want Fizz", results[2])
	}
	if results[4] != "Buzz" {
		t.Errorf("results[4] = %s, want Buzz", results[4])
	}
	if results[14] != "FizzBuzz" {
		t.Errorf("results[14] = %s, want FizzBuzz", results[14])
	}
}
