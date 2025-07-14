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
