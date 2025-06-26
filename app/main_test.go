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
