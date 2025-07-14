package main

import "testing"

func TestGreeting(t *testing.T) {
	expected := "hello world"
	actual := greeting()
	if actual != expected {
		t.Errorf("Expected %s, but got %s", expected, actual)
	}
}

func greeting() string {
	return "hello world"
}
