package main

import "testing"

func assertGenerate(t *testing.T, input int, expected string) {
    t.Helper()
    got := Generate(input)
    if got != expected {
        t.Errorf("Generate(%d) = %v, want %v", input, got, expected)
    }
}

func Test1を渡したら文字列1を返す(t *testing.T) {
    assertGenerate(t, 1, "1")
}

func Test2を渡したら文字列2を返す(t *testing.T) {
    assertGenerate(t, 2, "2")
}
