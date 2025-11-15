package model

import "time"

// PriceByCustomer は顧客別販売単価のドメインモデルです
type PriceByCustomer struct {
	ProductCode  string    `db:"商品コード"`
	CustomerCode string    `db:"取引先コード"`
	Price        int       `db:"販売単価"`
	CreatedAt    time.Time `db:"作成日時"`
	CreatedBy    string    `db:"作成者名"`
	UpdatedAt    time.Time `db:"更新日時"`
	UpdatedBy    string    `db:"更新者名"`
}
