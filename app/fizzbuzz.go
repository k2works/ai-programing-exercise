package main

import (
	"fmt"
	"strconv"
)

// InvalidValueError 不正な値エラー
type InvalidValueError struct {
	message string
}

func (e InvalidValueError) Error() string {
	return e.message
}

// NewInvalidValueError コンストラクタ
func NewInvalidValueError(message string) InvalidValueError {
	return InvalidValueError{message: message}
}

// FizzBuzzValue 値オブジェクト
type FizzBuzzValue struct {
	number int
	value  string
}

// NewFizzBuzzValue コンストラクタ
func NewFizzBuzzValue(number int, value string) FizzBuzzValue {
	if number < 0 {
		panic(NewInvalidValueError("値は正の値のみ許可"))
	}
	return FizzBuzzValue{
		number: number,
		value:  value,
	}
}

// Number 数値を取得
func (f FizzBuzzValue) Number() int {
	return f.number
}

// Value 値を取得
func (f FizzBuzzValue) Value() string {
	return f.value
}

// String 文字列表現
func (f FizzBuzzValue) String() string {
	return fmt.Sprintf("%d:%s", f.number, f.value)
}

// Equal 等価性チェック
func (f FizzBuzzValue) Equal(other FizzBuzzValue) bool {
	return f.number == other.number && f.value == other.value
}

// FizzBuzzType インターフェース
type FizzBuzzType interface {
	Generate(number int) FizzBuzzValue
}

// FizzBuzzTypeBase 共通処理を提供する基底構造体
type FizzBuzzTypeBase struct{}

// タイプ定数
const (
	TYPE_01 = 1
	TYPE_02 = 2
	TYPE_03 = 3
)

// Create ファクトリメソッド
func (f FizzBuzzTypeBase) Create(fizzBuzzType int) FizzBuzzType {
	switch fizzBuzzType {
	case TYPE_01:
		return FizzBuzzType01{}
	case TYPE_02:
		return FizzBuzzType02{}
	case TYPE_03:
		return FizzBuzzType03{}
	default:
		panic("該当するタイプは存在しません")
	}
}

func (f FizzBuzzTypeBase) Fizz(number int) bool {
	return number%3 == 0
}

func (f FizzBuzzTypeBase) Buzz(number int) bool {
	return number%5 == 0
}

// FizzBuzzType01 タイプ1の実装
type FizzBuzzType01 struct {
	FizzBuzzTypeBase
}

func (f FizzBuzzType01) Generate(number int) FizzBuzzValue {
	isFizz := f.Fizz(number)
	isBuzz := f.Buzz(number)

	if isFizz && isBuzz {
		return NewFizzBuzzValue(number, "FizzBuzz")
	}
	if isFizz {
		return NewFizzBuzzValue(number, "Fizz")
	}
	if isBuzz {
		return NewFizzBuzzValue(number, "Buzz")
	}
	return NewFizzBuzzValue(number, strconv.Itoa(number))
}

// FizzBuzzType02 タイプ2の実装
type FizzBuzzType02 struct {
	FizzBuzzTypeBase
}

func (f FizzBuzzType02) Generate(number int) FizzBuzzValue {
	return NewFizzBuzzValue(number, strconv.Itoa(number))
}

// FizzBuzzType03 タイプ3の実装
type FizzBuzzType03 struct {
	FizzBuzzTypeBase
}

func (f FizzBuzzType03) Generate(number int) FizzBuzzValue {
	isFizz := f.Fizz(number)
	isBuzz := f.Buzz(number)

	if isFizz && isBuzz {
		return NewFizzBuzzValue(number, "FizzBuzz")
	}
	return NewFizzBuzzValue(number, strconv.Itoa(number))
}

// FizzBuzz構造体
type FizzBuzz struct {
	list         *FizzBuzzList // FizzBuzzListを保持するフィールド
	fizzBuzzType int           // FizzBuzzのタイプを保持するフィールド
	typeImpl     FizzBuzzType  // タイプ実装
}

// NewFizzBuzz コンストラクタ（プリミティブ型を受け取る）
func NewFizzBuzz(fizzBuzzType int) *FizzBuzz {
	base := FizzBuzzTypeBase{}
	typeImpl := base.Create(fizzBuzzType)

	return &FizzBuzz{
		list:         NewFizzBuzzList([]FizzBuzzValue{}),
		fizzBuzzType: fizzBuzzType,
		typeImpl:     typeImpl,
	}
}

// NewFizzBuzzWithType コンストラクタ（値オブジェクトを受け取る）
func NewFizzBuzzWithType(typeImpl FizzBuzzType) *FizzBuzz {
	// タイプを逆算（実際の実装では避けるべきですが、ここでは後方互換性のため）
	var fizzBuzzType int
	switch typeImpl.(type) {
	case FizzBuzzType01:
		fizzBuzzType = TYPE_01
	case FizzBuzzType02:
		fizzBuzzType = TYPE_02
	case FizzBuzzType03:
		fizzBuzzType = TYPE_03
	default:
		panic("該当するタイプは存在しません")
	}

	return &FizzBuzz{
		list:         NewFizzBuzzList([]FizzBuzzValue{}),
		fizzBuzzType: fizzBuzzType,
		typeImpl:     typeImpl,
	}
}

// Generate 数を文字列に変換してFizzBuzzルールを適用
func (f *FizzBuzz) Generate(number int) FizzBuzzValue {
	return f.typeImpl.Generate(number)
}

// Type FizzBuzzタイプを取得
func (f *FizzBuzz) Type() int {
	return f.fizzBuzzType
}

// List FizzBuzz配列を取得
func (f *FizzBuzz) List() []FizzBuzzValue {
	return f.list.Value()
}

// GenerateList 範囲指定してFizzBuzzのリストを作成し、インスタンス変数に保存
func (f *FizzBuzz) GenerateList(start, end int) {
	values := make([]FizzBuzzValue, 0, end-start+1)
	for i := start; i <= end; i++ {
		values = append(values, f.Generate(i))
	}
	f.list = f.list.Add(values)
}

// FizzBuzzList ファーストクラスコレクション
type FizzBuzzList struct {
	value []FizzBuzzValue
}

// NewFizzBuzzList コンストラクタ
func NewFizzBuzzList(values []FizzBuzzValue) *FizzBuzzList {
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

// FizzBuzzCommand Commandパターンのインターフェース
type FizzBuzzCommand interface {
	Execute(number int) interface{}
}

// FizzBuzzValueCommand 値オブジェクトを返すコマンド
type FizzBuzzValueCommand struct {
	typeImpl FizzBuzzType
}

// NewFizzBuzzValueCommand コンストラクタ
func NewFizzBuzzValueCommand(typeImpl FizzBuzzType) *FizzBuzzValueCommand {
	return &FizzBuzzValueCommand{
		typeImpl: typeImpl,
	}
}

// Execute 値オブジェクトの値部分を返す
func (c *FizzBuzzValueCommand) Execute(number int) interface{} {
	return c.typeImpl.Generate(number).Value()
}

// FizzBuzzListCommand ファーストクラスコレクションを返すコマンド
type FizzBuzzListCommand struct {
	typeImpl FizzBuzzType
}

// NewFizzBuzzListCommand コンストラクタ
func NewFizzBuzzListCommand(typeImpl FizzBuzzType) *FizzBuzzListCommand {
	return &FizzBuzzListCommand{
		typeImpl: typeImpl,
	}
}

// Execute 指定した数までのFizzBuzzリストを生成して返す
func (c *FizzBuzzListCommand) Execute(number int) interface{} {
	const MaxCount = 100
	if number > MaxCount {
		panic(NewInvalidValueError(fmt.Sprintf("%d より多い数を許可しない", MaxCount)))
	}
	values := make([]FizzBuzzValue, 0, number)
	for i := 1; i <= number; i++ {
		values = append(values, c.typeImpl.Generate(i))
	}
	return NewFizzBuzzList(values).Value()
}


