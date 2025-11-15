package model

import "time"

// Warehouse は倉庫マスタのドメインモデルです
type Warehouse struct {
	WarehouseCode string    `db:"倉庫コード"`
	WarehouseName string    `db:"倉庫名"`
	PostalCode    string    `db:"郵便番号"`
	Prefecture    string    `db:"都道府県"`
	Address1      string    `db:"住所１"`
	Address2      string    `db:"住所２"`
	PhoneNumber   string    `db:"電話番号"`
	FaxNumber     string    `db:"fax番号"`
	CreatedAt     time.Time `db:"作成日時"`
	CreatedBy     string    `db:"作成者名"`
	UpdatedAt     time.Time `db:"更新日時"`
	UpdatedBy     string    `db:"更新者名"`
}
