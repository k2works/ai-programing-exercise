package model

import (
	"database/sql"
	"time"
)

// Sales 売上のドメインモデル（ヘッダー）
type Sales struct {
	SalesNo        string         `db:"売上番号"`
	SalesDate      sql.NullTime   `db:"売上日"`
	SalesCategory  int            `db:"売上区分"` // 1:通常売上、2:返品、3:値引
	OrderNo        sql.NullString `db:"受注番号"`
	DepartmentCode string         `db:"部門コード"`
	StartDate      time.Time      `db:"開始日"`
	CompanyCode    string         `db:"取引先コード"`
	SalesAmount    int            `db:"売上金額"`
	ConsumptionTax int            `db:"消費税"`
	CorrectionNo   sql.NullString `db:"訂正番号"`  // 赤黒伝票の黒伝票番号
	OriginalSlipNo sql.NullString `db:"元伝票番号"` // 赤黒伝票の元伝票番号
	CreatedAt      time.Time      `db:"作成日時"`
	CreatedBy      string         `db:"作成者名"`
	UpdatedAt      time.Time      `db:"更新日時"`
	UpdatedBy      string         `db:"更新者名"`
}

// SalesDetail 売上明細のドメインモデル
type SalesDetail struct {
	SalesNo          string         `db:"売上番号"`
	DetailNo         int            `db:"明細番号"`
	ProductCode      string         `db:"商品コード"`
	ProductName      sql.NullString `db:"商品名"`
	SellingPrice     int            `db:"販売単価"`
	ShippedQuantity  int            `db:"出荷済数量"`
	Quantity         int            `db:"数量"`
	DiscountAmount   int            `db:"値引額"`
	BillingDate      sql.NullTime   `db:"請求日"`
	InvoiceNo        sql.NullString `db:"請求番号"`
	BillingDelayFlag sql.NullInt32  `db:"請求遅延区分"`
	AutoJournalDate  sql.NullTime   `db:"自動仕訳日"`
	CreatedAt        time.Time      `db:"作成日時"`
	CreatedBy        string         `db:"作成者名"`
	UpdatedAt        time.Time      `db:"更新日時"`
	UpdatedBy        string         `db:"更新者名"`
}
