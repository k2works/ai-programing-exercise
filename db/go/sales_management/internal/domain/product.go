package domain

import "time"

// Product は API レイヤー用の商品ドメインモデルです
type Product struct {
	ProdCode     string    `db:"商品コード"`
	FullName     string    `db:"商品正式名"`
	Name         string    `db:"商品略称"`
	Kana         *string   `db:"商品名カナ"`
	UnitPrice    int       `db:"販売単価"`
	PoPrice      int       `db:"仕入単価"`
	SupCode      *string   `db:"仕入先コード"`
	CategoryCode *string   `db:"商品分類コード"`
	CreateDate   time.Time `db:"作成日時"`
	Creator      *string   `db:"作成者名"`
	UpdateDate   time.Time `db:"更新日時"`
	Updater      *string   `db:"更新者名"`
}
