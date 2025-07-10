package model

import "fmt"

// FizzBuzzList ファーストクラスコレクション
type FizzBuzzList struct {
	value []FizzBuzzValue
}

// MAX_COUNT 最大件数
const MAX_COUNT = 100

// NewFizzBuzzList コンストラクタ
func NewFizzBuzzList(values []FizzBuzzValue) *FizzBuzzList {
	if len(values) > MAX_COUNT {
		panic(NewInvalidValueError(fmt.Sprintf("上限は%d件までです", MAX_COUNT)))
	}
	// イミュータブルにするため新しいスライスを作成
	newValues := make([]FizzBuzzValue, len(values))
	copy(newValues, values)
	return &FizzBuzzList{
		value: newValues,
	}
}

// Value 値を取得（読み取り専用）
func (f *FizzBuzzList) Value() []FizzBuzzValue {
	// 外部からの変更を防ぐためコピーを返す
	result := make([]FizzBuzzValue, len(f.value))
	copy(result, f.value)
	return result
}

// String 文字列表現
func (f *FizzBuzzList) String() string {
	return fmt.Sprintf("%v", f.value)
}

// Add 新しい要素を追加した新しいFizzBuzzListを返す（イミュータブル）
func (f *FizzBuzzList) Add(values []FizzBuzzValue) *FizzBuzzList {
	newValues := make([]FizzBuzzValue, len(f.value)+len(values))
	copy(newValues, f.value)
	copy(newValues[len(f.value):], values)
	return &FizzBuzzList{
		value: newValues,
	}
}

// Count 要素数を取得
func (f *FizzBuzzList) Count() int {
	return len(f.value)
}
