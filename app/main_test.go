package main

import "testing"

var fizzbuzz func(int) string

func setup() {
	fizzbuzz = FizzBuzzGenerate
}

func Test1を渡したら文字列1を返す(t *testing.T) {
	setup()
	expected := "1"
	actual := fizzbuzz(1)
	if actual != expected {
		t.Errorf("Expected %s, but got %s", expected, actual)
	}
}

func Test2を渡したら文字列2を返す(t *testing.T) {
	setup()
	expected := "2"
	actual := fizzbuzz(2)
	if actual != expected {
		t.Errorf("Expected %s, but got %s", expected, actual)
	}
}

func Test3を渡したら文字列Fizzを返す(t *testing.T) {
	setup()
	expected := "Fizz"
	actual := fizzbuzz(3)
	if actual != expected {
		t.Errorf("Expected %s, but got %s", expected, actual)
	}
}

func Test5を渡したら文字列Buzzを返す(t *testing.T) {
	setup()
	expected := "Buzz"
	actual := fizzbuzz(5)
	if actual != expected {
		t.Errorf("Expected %s, but got %s", expected, actual)
	}
}

func Test15を渡したら文字列FizzBuzzを返す(t *testing.T) {
	setup()
	expected := "FizzBuzz"
	actual := fizzbuzz(15)
	if actual != expected {
		t.Errorf("Expected %s, but got %s", expected, actual)
	}
}

func Test1から100までのFizzBuzz配列を返す(t *testing.T) {
	setup()
	result := make([]string, 100)
	for i := 1; i <= 100; i++ {
		result[i-1] = fizzbuzz(i)
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
}
