package model

import "time"

// ProductCategory は商品分類マスタのドメインモデルです
// 商品を階層的に分類するためのマスタデータ
type ProductCategory struct {
	CategoryCode  string    `db:"商品分類コード"`
	CategoryName  string    `db:"商品分類名"`
	CategoryLevel int       `db:"商品分類階層"` // 1が最上位
	CategoryPath  string    `db:"商品分類パス"` // 例: /PC/PC01/
	IsLowestLevel int       `db:"最下層区分"`  // 0:中間層、1:最下層
	CreatedAt     time.Time `db:"作成日時"`
	CreatedBy     string    `db:"作成者名"`
	UpdatedAt     time.Time `db:"更新日時"`
	UpdatedBy     string    `db:"更新者名"`
}
