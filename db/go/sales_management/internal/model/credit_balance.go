package model

import (
	"time"
)

// CreditBalance 与信残高のドメインモデル
type CreditBalance struct {
	CompanyCode        string    `db:"取引先コード"`
	OrderBalance       int       `db:"受注残高"`
	AccountsReceivable int       `db:"売掛残高"`
	AccountsPayable    int       `db:"買掛残高"`
	CreatedAt          time.Time `db:"作成日時"`
	CreatedBy          string    `db:"作成者名"`
	UpdatedAt          time.Time `db:"更新日時"`
	UpdatedBy          string    `db:"更新者名"`
}
