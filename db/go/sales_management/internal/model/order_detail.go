package model

import "time"

// OrderDetail は受注データ明細のドメインモデルです
// 受注に含まれる各商品の情報を保持する（明細）
type OrderDetail struct {
	OrderNo               string    `db:"受注番号"`
	DetailNo              int       `db:"明細番号"`
	ProductCode           string    `db:"商品コード"`
	ProductName           string    `db:"商品名"`
	SalesUnitPrice        int       `db:"販売単価"`
	Quantity              int       `db:"数量"`
	ConsumptionTaxRate    int       `db:"消費税率"`
	AllocatedQuantity     int       `db:"引当数量"`
	ShippingOrderQuantity int       `db:"出荷指示数量"`
	ShippedQuantity       int       `db:"出荷済数量"`
	CompletionFlag        int       `db:"完了フラグ"`
	DiscountAmount        int       `db:"値引額"`
	DeliveryDate          time.Time `db:"納品日"`
	CreatedAt             time.Time `db:"作成日時"`
	CreatedBy             string    `db:"作成者名"`
	UpdatedAt             time.Time `db:"更新日時"`
	UpdatedBy             string    `db:"更新者名"`
}
