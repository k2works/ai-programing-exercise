package main

import "testing"

// 数を文字列にして返す
// タイプ1の場合
func Test_タイプ1_1を渡したら文字列1を返す(t *testing.T) {
	expected := "1"
	actual := FizzBuzzGenerateWithType(1, 1)
	if actual != expected {
		t.Errorf("Expected %s, but got %s", expected, actual)
	}
}

func Test_タイプ1_3を渡したら文字列Fizzを返す(t *testing.T) {
	expected := "Fizz"
	actual := FizzBuzzGenerateWithType(3, 1)
	if actual != expected {
		t.Errorf("Expected %s, but got %s", expected, actual)
	}
}

func Test_タイプ1_5を渡したら文字列Buzzを返す(t *testing.T) {
	expected := "Buzz"
	actual := FizzBuzzGenerateWithType(5, 1)
	if actual != expected {
		t.Errorf("Expected %s, but got %s", expected, actual)
	}
}

func Test_タイプ1_15を渡したら文字列FizzBuzzを返す(t *testing.T) {
	expected := "FizzBuzz"
	actual := FizzBuzzGenerateWithType(15, 1)
	if actual != expected {
		t.Errorf("Expected %s, but got %s", expected, actual)
	}
}

// タイプ2の場合（数字のみ）
func Test_タイプ2_1を渡したら文字列1を返す(t *testing.T) {
	expected := "1"
	actual := FizzBuzzGenerateWithType(1, 2)
	if actual != expected {
		t.Errorf("Expected %s, but got %s", expected, actual)
	}
}

func Test_タイプ2_3を渡したら文字列3を返す(t *testing.T) {
	expected := "3"
	actual := FizzBuzzGenerateWithType(3, 2)
	if actual != expected {
		t.Errorf("Expected %s, but got %s", expected, actual)
	}
}

func Test_タイプ2_5を渡したら文字列5を返す(t *testing.T) {
	expected := "5"
	actual := FizzBuzzGenerateWithType(5, 2)
	if actual != expected {
		t.Errorf("Expected %s, but got %s", expected, actual)
	}
}

func Test_タイプ2_15を渡したら文字列15を返す(t *testing.T) {
	expected := "15"
	actual := FizzBuzzGenerateWithType(15, 2)
	if actual != expected {
		t.Errorf("Expected %s, but got %s", expected, actual)
	}
}

// タイプ3の場合（FizzBuzzのみ）
func Test_タイプ3_1を渡したら文字列1を返す(t *testing.T) {
	expected := "1"
	actual := FizzBuzzGenerateWithType(1, 3)
	if actual != expected {
		t.Errorf("Expected %s, but got %s", expected, actual)
	}
}

func Test_タイプ3_3を渡したら文字列3を返す(t *testing.T) {
	expected := "3"
	actual := FizzBuzzGenerateWithType(3, 3)
	if actual != expected {
		t.Errorf("Expected %s, but got %s", expected, actual)
	}
}

func Test_タイプ3_5を渡したら文字列5を返す(t *testing.T) {
	expected := "5"
	actual := FizzBuzzGenerateWithType(5, 3)
	if actual != expected {
		t.Errorf("Expected %s, but got %s", expected, actual)
	}
}

func Test_タイプ3_15を渡したら文字列FizzBuzzを返す(t *testing.T) {
	expected := "FizzBuzz"
	actual := FizzBuzzGenerateWithType(15, 3)
	if actual != expected {
		t.Errorf("Expected %s, but got %s", expected, actual)
	}
}

// 既存のテスト（後で移行）
func Test1を渡したら文字列1を返す(t *testing.T) {
	expected := "1"
	actual := FizzBuzzGenerate(1)
	if actual != expected {
		t.Errorf("Expected %s, but got %s", expected, actual)
	}
}

func Test2を渡したら文字列2を返す(t *testing.T) {
	expected := "2"
	actual := FizzBuzzGenerate(2)
	if actual != expected {
		t.Errorf("Expected %s, but got %s", expected, actual)
	}
}

func Test3を渡したら文字列Fizzを返す(t *testing.T) {
	expected := "Fizz"
	actual := FizzBuzzGenerate(3)
	if actual != expected {
		t.Errorf("Expected %s, but got %s", expected, actual)
	}
}

func Test5を渡したら文字列Buzzを返す(t *testing.T) {
	expected := "Buzz"
	actual := FizzBuzzGenerate(5)
	if actual != expected {
		t.Errorf("Expected %s, but got %s", expected, actual)
	}
}

func Test15を渡したら文字列FizzBuzzを返す(t *testing.T) {
	expected := "FizzBuzz"
	actual := FizzBuzzGenerate(15)
	if actual != expected {
		t.Errorf("Expected %s, but got %s", expected, actual)
	}
}

func Test1から100までのFizzBuzz配列を返す(t *testing.T) {
	result := make([]string, 100)
	for i := 1; i <= 100; i++ {
		result[i-1] = FizzBuzzGenerate(i)
	}

	// 最初の数項と特定の値をテスト
	if result[0] != "1" {
		t.Errorf("Expected result[0] to be '1', but got '%s'", result[0])
	}
	if result[2] != "Fizz" {
		t.Errorf("Expected result[2] to be 'Fizz', but got '%s'", result[2])
	}
	if result[4] != "Buzz" {
		t.Errorf("Expected result[4] to be 'Buzz', but got '%s'", result[4])
	}
	if result[14] != "FizzBuzz" {
		t.Errorf("Expected result[14] to be 'FizzBuzz', but got '%s'", result[14])
	}
	if result[99] != "Buzz" {
		t.Errorf("Expected result[99] to be 'Buzz', but got '%s'", result[99])
	}
}

// それ以外のタイプの場合（例外処理）
func Test_それ以外のタイプの場合_例外を返す(t *testing.T) {
	// Go では panic/recover を使って例外処理をテストします
	defer func() {
		if r := recover(); r == nil {
			t.Errorf("Expected panic for invalid type, but no panic occurred")
		}
	}()

	// 不正なタイプ4を渡すとpanicが発生することを期待
	FizzBuzzGenerateWithType(1, 4)
}

// オブジェクト指向版のテスト（FizzBuzz構造体）
func Test_FizzBuzz構造体_タイプ1_1を渡したら文字列1を返す(t *testing.T) {
	fizzbuzz := NewFizzBuzz(1)
	expected := "1"
	actual := fizzbuzz.Generate(1)
	if actual != expected {
		t.Errorf("Expected %s, but got %s", expected, actual)
	}
}

func Test_FizzBuzz構造体_タイプ1_3を渡したら文字列Fizzを返す(t *testing.T) {
	fizzbuzz := NewFizzBuzz(1)
	expected := "Fizz"
	actual := fizzbuzz.Generate(3)
	if actual != expected {
		t.Errorf("Expected %s, but got %s", expected, actual)
	}
}

func Test_FizzBuzz構造体_タイプ2_3を渡したら文字列3を返す(t *testing.T) {
	fizzbuzz := NewFizzBuzz(2)
	expected := "3"
	actual := fizzbuzz.Generate(3)
	if actual != expected {
		t.Errorf("Expected %s, but got %s", expected, actual)
	}
}

func Test_FizzBuzz構造体_タイプ3_15を渡したら文字列FizzBuzzを返す(t *testing.T) {
	fizzbuzz := NewFizzBuzz(3)
	expected := "FizzBuzz"
	actual := fizzbuzz.Generate(15)
	if actual != expected {
		t.Errorf("Expected %s, but got %s", expected, actual)
	}
}

func Test_FizzBuzz構造体_配列を返す(t *testing.T) {
	fizzbuzz := NewFizzBuzzDefault()
	fizzbuzz.GenerateList()
	result := fizzbuzz.List()

	// 配列の長さをテスト
	if len(result) != 100 {
		t.Errorf("Expected length 100, but got %d", len(result))
	}

	// 最初の数項と特定の値をテスト
	if result[0] != "1" {
		t.Errorf("Expected result[0] to be '1', but got '%s'", result[0])
	}
	if result[2] != "Fizz" {
		t.Errorf("Expected result[2] to be 'Fizz', but got '%s'", result[2])
	}
	if result[4] != "Buzz" {
		t.Errorf("Expected result[4] to be 'Buzz', but got '%s'", result[4])
	}
	if result[14] != "FizzBuzz" {
		t.Errorf("Expected result[14] to be 'FizzBuzz', but got '%s'", result[14])
	}
	if result[99] != "Buzz" {
		t.Errorf("Expected result[99] to be 'Buzz', but got '%s'", result[99])
	}
}
