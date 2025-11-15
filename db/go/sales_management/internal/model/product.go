package model

import "time"

// Product は商品マスタのドメインモデルです
type Product struct {
	ProductCode         string    `db:"商品コード"`
	ProductFullName     string    `db:"商品正式名"`
	ProductAbbreviation string    `db:"商品略称"`
	ProductNameKana     string    `db:"商品名カナ"`
	ProductType         string    `db:"商品区分"`
	ModelNumber         string    `db:"製品型番"`
	SellingPrice        int       `db:"販売単価"`
	PurchasePrice       int       `db:"仕入単価"`
	CostOfSales         int       `db:"売上原価"`
	TaxCategory         int       `db:"税区分"`
	ProductCategoryCode string    `db:"商品分類コード"`
	MiscCategory        int       `db:"雑区分"`
	InventoryManaged    int       `db:"在庫管理対象区分"`
	StockAllocation     int       `db:"在庫引当区分"`
	SupplierCode        string    `db:"仕入先コード"`
	SupplierBranch      int       `db:"仕入先枝番"`
	CreatedAt           time.Time `db:"作成日時"`
	CreatedBy           string    `db:"作成者名"`
	UpdatedAt           time.Time `db:"更新日時"`
	UpdatedBy           string    `db:"更新者名"`
}
