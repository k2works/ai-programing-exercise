package model

import "time"

// Supplier は仕入先マスタのドメインモデルです
// 取引先のうち、仕入先としての詳細情報を保持する
type Supplier struct {
	SupplierCode           string    `db:"仕入先コード"`
	SupplierBranch         int       `db:"仕入先枝番"`
	SupplierCategory       int       `db:"仕入先区分"`
	SupplierName           string    `db:"仕入先名"`
	SupplierNameKana       string    `db:"仕入先名カナ"`
	EmployeeCode           string    `db:"担当社員コード"`
	SupplierContactName    string    `db:"仕入先担当者名"`
	SupplierDepartmentName string    `db:"仕入先担当部署名"`
	SupplierPostalCode     string    `db:"仕入先郵便番号"`
	SupplierPrefecture     string    `db:"仕入先都道府県"`
	SupplierAddress1       string    `db:"仕入先住所１"`
	SupplierPhone          string    `db:"仕入先電話番号"`
	SupplierEmail          string    `db:"仕入先メールアドレス"`
	ClosingDay             int       `db:"仕入先締日"`
	PaymentMonth           int       `db:"仕入先支払月数"`
	PaymentDay             int       `db:"仕入先支払日"`
	PaymentMethod          int       `db:"仕入先支払方法"`
	CreatedAt              time.Time `db:"作成日時"`
	CreatedBy              string    `db:"作成者名"`
	UpdatedAt              time.Time `db:"更新日時"`
	UpdatedBy              string    `db:"更新者名"`
}
