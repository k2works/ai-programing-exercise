package model

// FizzBuzzValue 値オブジェクト（Value Object パターン）
type FizzBuzzValue struct {
	number int
	value  string
}

// NewFizzBuzzValue FizzBuzzValueのコンストラクタ
func NewFizzBuzzValue(number int, value string) *FizzBuzzValue {
	return &FizzBuzzValue{number: number, value: value}
}

// Number ゲッターメソッド
func (fv *FizzBuzzValue) Number() int {
	return fv.number
}

// Value ゲッターメソッド
func (fv *FizzBuzzValue) Value() string {
	return fv.value
}

// ToString 文字列表現を返す
func (fv *FizzBuzzValue) ToString() string {
	return fv.value
}

// Equal 等価性を比較
func (fv *FizzBuzzValue) Equal(other *FizzBuzzValue) bool {
	return fv.number == other.number && fv.value == other.value
}
