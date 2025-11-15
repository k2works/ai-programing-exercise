package model

import "time"

// SalesOrder は受注データのドメインモデルです
// 顧客からの受注全体の情報を保持する（ヘッダ）
type SalesOrder struct {
	OrderNo         string    `db:"受注番号"`
	OrderDate       time.Time `db:"受注日"`
	DepartmentCode  string    `db:"部門コード"`
	StartDate       time.Time `db:"開始日"`
	CustomerCode    string    `db:"顧客コード"`
	CustomerBranch  int       `db:"顧客枝番"`
	EmployeeCode    string    `db:"社員コード"`
	DeliveryDate    time.Time `db:"納期"`
	CustomerOrderNo string    `db:"顧客注文番号"`
	WarehouseCode   string    `db:"倉庫コード"`
	OrderAmount     int       `db:"受注金額"`
	ConsumptionTax  int       `db:"消費税"`
	DocumentComment string    `db:"伝票コメント"`
	CreatedAt       time.Time `db:"作成日時"`
	CreatedBy       string    `db:"作成者名"`
	UpdatedAt       time.Time `db:"更新日時"`
	UpdatedBy       string    `db:"更新者名"`
}
