package model

import "time"

// AlternateProduct は代替商品のドメインモデルです
type AlternateProduct struct {
	ProductCode          string    `db:"商品コード"`
	AlternateProductCode string    `db:"代替商品コード"`
	Priority             int       `db:"優先順位"`
	CreatedAt            time.Time `db:"作成日時"`
	CreatedBy            string    `db:"作成者名"`
	UpdatedAt            time.Time `db:"更新日時"`
	UpdatedBy            string    `db:"更新者名"`
}
