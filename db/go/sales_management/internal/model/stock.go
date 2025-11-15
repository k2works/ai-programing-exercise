package model

import (
	"database/sql"
	"time"
)

// Stock は在庫データのドメインモデルです
// 倉庫コード、商品コード、ロット番号、在庫区分、品質区分の5つのフィールドで主キーを構成
type Stock struct {
	WarehouseCode    string       `db:"倉庫コード"`
	ProductCode      string       `db:"商品コード"`
	LotNumber        string       `db:"ロット番号"`
	StockCategory    string       `db:"在庫区分"`  // 1:通常在庫、2:預かり在庫、3:委託在庫
	QualityCategory  string       `db:"品質区分"`  // A:良品、B:不良品、C:検査中
	ActualStock      int          `db:"実在庫数"`  // 物理的に存在する在庫数
	AvailableStock   int          `db:"有効在庫数"` // 販売可能な在庫数
	LastShippingDate sql.NullTime `db:"最終出荷日"`
	CreatedAt        time.Time    `db:"作成日時"`
	CreatedBy        string       `db:"作成者名"`
	UpdatedAt        time.Time    `db:"更新日時"`
	UpdatedBy        string       `db:"更新者名"`
}
