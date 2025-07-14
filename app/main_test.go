package main

import "testing"

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
