package main

import "testing"

func TestGreeting(t *testing.T) {
    got := greeting()
    want := "hello world"
    if got != want {
        t.Errorf("greeting() = %v, want %v", got, want)
    }
}
