package model

import "time"

// Company は取引先マスタのドメインモデルです
// すべての取引先の基本情報を保持する
// 顧客、仕入先などの役割に応じた詳細情報は別テーブルで管理
type Company struct {
	CompanyCode          string    `db:"取引先コード"`
	CompanyName          string    `db:"取引先名"`
	CompanyNameKana      string    `db:"取引先名カナ"`
	SupplierCategory     int       `db:"仕入先区分"`
	PostalCode           string    `db:"郵便番号"`
	Prefecture           string    `db:"都道府県"`
	Address1             string    `db:"住所１"`
	Address2             string    `db:"住所２"`
	TransactionProhibit  int       `db:"取引禁止フラグ"`
	MiscCategory         int       `db:"雑区分"`
	CompanyGroupCode     string    `db:"取引先グループコード"`
	CreditLimit          int       `db:"与信限度額"`
	TemporaryCreditBoost int       `db:"与信一時増加枠"`
	CreatedAt            time.Time `db:"作成日時"`
	CreatedBy            string    `db:"作成者名"`
	UpdatedAt            time.Time `db:"更新日時"`
	UpdatedBy            string    `db:"更新者名"`
}
