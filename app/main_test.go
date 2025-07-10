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

// タイプ2の場合

// その他の場合
func Test1を渡したら文字列1を返す_タイプ2(t *testing.T) {
	assertGenerateByType(t, 1, 2, "1")
}

// 3の倍数のときは数を文字列にして返す
func Test3を渡したら文字列3を返す_タイプ2(t *testing.T) {
	assertGenerateByType(t, 3, 2, "3")
}

// 5の倍数のときは数を文字列にして返す
func Test5を渡したら文字列5を返す_タイプ2(t *testing.T) {
	assertGenerateByType(t, 5, 2, "5")
}

// 3と5両方の倍数の場合には数を文字列にして返す
func Test15を渡したら文字列15を返す_タイプ2(t *testing.T) {
	assertGenerateByType(t, 15, 2, "15")
}

// タイプ3の場合

// その他の場合
func Test1を渡したら文字列1を返す_タイプ3(t *testing.T) {
	assertGenerateByType(t, 1, 3, "1")
}

// 3の倍数のときは数を文字列にして返す
func Test3を渡したら文字列3を返す_タイプ3(t *testing.T) {
	assertGenerateByType(t, 3, 3, "3")
}

// 5の倍数のときは数を文字列にして返す
func Test5を渡したら文字列5を返す_タイプ3(t *testing.T) {
	assertGenerateByType(t, 5, 3, "5")
}

// 3と5両方の倍数の場合にはFizzBuzzと返す
func Test15を渡したら文字列FizzBuzzを返す_タイプ3(t *testing.T) {
	assertGenerateByType(t, 15, 3, "FizzBuzz")
}

// それ以外のタイプの場合
func TestGenerateByType_それ以外のタイプで例外が発生する(t *testing.T) {
	defer func() {
		if r := recover(); r == nil {
			t.Errorf("GenerateByType(1, 4) should panic")
		}
	}()
	GenerateByType(1, 4)
}
