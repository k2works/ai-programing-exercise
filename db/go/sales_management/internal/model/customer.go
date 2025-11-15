package model

import "time"

// Customer は顧客マスタのドメインモデルです
// 取引先のうち、顧客としての詳細情報を保持する
type Customer struct {
	CustomerCode           string    `db:"顧客コード"`
	CustomerBranch         int       `db:"顧客枝番"`
	CustomerCategory       int       `db:"顧客区分"`
	BillingCode            string    `db:"請求先コード"`
	BillingBranch          int       `db:"請求先枝番"`
	CollectionCode         string    `db:"回収先コード"`
	CollectionBranch       int       `db:"回収先枝番"`
	CustomerName           string    `db:"顧客名"`
	CustomerNameKana       string    `db:"顧客名カナ"`
	SalesEmployeeCode      string    `db:"担当社員コード"`
	CustomerContactName    string    `db:"顧客担当者名"`
	CustomerDepartmentName string    `db:"顧客担当部署名"`
	CustomerPostalCode     string    `db:"顧客郵便番号"`
	CustomerPrefecture     string    `db:"顧客都道府県"`
	CustomerAddress1       string    `db:"顧客住所１"`
	CustomerPhone          string    `db:"顧客電話番号"`
	CustomerEmail          string    `db:"顧客メールアドレス"`
	ClosingDay1            int       `db:"顧客締日１"`
	PaymentMonth1          int       `db:"顧客支払月数１"`
	PaymentDay1            int       `db:"顧客支払日１"`
	PaymentMethod1         int       `db:"顧客支払方法１"`
	ClosingDay2            int       `db:"顧客締日２"`
	CreatedAt              time.Time `db:"作成日時"`
	CreatedBy              string    `db:"作成者名"`
	UpdatedAt              time.Time `db:"更新日時"`
	UpdatedBy              string    `db:"更新者名"`
}
