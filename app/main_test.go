package main

import "testing"

func Test1を渡したら文字列1を返す(t *testing.T) {
    got := Generate(1)
    want := "1"
    if got != want {
        t.Errorf("Generate(1) = %v, want %v", got, want)
    }
}

func Test2を渡したら文字列2を返す(t *testing.T) {
    got := Generate(2)
    want := "2"
    if got != want {
        t.Errorf("Generate(2) = %v, want %v", got, want)
    }
}
