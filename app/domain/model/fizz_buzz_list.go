package model

import "fmt"

// FizzBuzzList ファーストクラスコレクション（コレクションのカプセル化）
type FizzBuzzList struct {
	values []*FizzBuzzValue
}

// NewFizzBuzzList FizzBuzzListのコンストラクタ
func NewFizzBuzzList(values []*FizzBuzzValue) *FizzBuzzList {
	return &FizzBuzzList{values: values}
}

// NewFizzBuzzListEmpty 空のFizzBuzzListのコンストラクタ
func NewFizzBuzzListEmpty() *FizzBuzzList {
	return &FizzBuzzList{values: make([]*FizzBuzzValue, 0)}
}

// Values ゲッターメソッド
func (fl *FizzBuzzList) Values() []*FizzBuzzValue {
	return fl.values
}

// ToString 文字列表現を返す
func (fl *FizzBuzzList) ToString() string {
	result := make([]string, len(fl.values))
	for i, value := range fl.values {
		result[i] = value.Value()
	}
	return fmt.Sprintf("%v", result)
}

// Add 新しい値を追加した新しいFizzBuzzListを返す（イミュータブル）
func (fl *FizzBuzzList) Add(values []*FizzBuzzValue) *FizzBuzzList {
	newValues := make([]*FizzBuzzValue, len(fl.values)+len(values))
	copy(newValues, fl.values)
	copy(newValues[len(fl.values):], values)
	return NewFizzBuzzList(newValues)
}

// Count リストの要素数を返す
func (fl *FizzBuzzList) Count() int {
	return len(fl.values)
}

// Get 指定したインデックスの要素を返す
func (fl *FizzBuzzList) Get(index int) *FizzBuzzValue {
	if index < 0 || index >= len(fl.values) {
		panic("インデックスが範囲外です")
	}
	return fl.values[index]
}
