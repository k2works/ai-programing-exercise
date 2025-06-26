package main

import "testing"

// テスト関数：1を渡したら文字列"1"を返す
func Test1を渡したら文字列1を返す(t *testing.T) {
	want := "1"
	got := Generate(1)
	if got != want {
		t.Errorf("Generate(1) = %v, want %v", got, want)
	}
}

// テスト関数：2を渡したら文字列"2"を返す
func Test2を渡したら文字列2を返す(t *testing.T) {
	want := "2"
	got := Generate(2)
	if got != want {
		t.Errorf("Generate(2) = %v, want %v", got, want)
	}
}
