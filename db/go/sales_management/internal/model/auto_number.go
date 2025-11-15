package model

import (
	"time"
)

// AutoNumber 自動採番のドメインモデル
type AutoNumber struct {
	SlipType       string    `db:"伝票種別"`
	YearMonth      time.Time `db:"年月"`
	LastSlipNumber int       `db:"最終伝票番号"`
}
