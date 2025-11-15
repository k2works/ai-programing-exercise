package model

import "time"

// CompanyGroup は取引先グループマスタのドメインモデルです
type CompanyGroup struct {
	GroupCode string    `db:"取引先グループコード"`
	GroupName string    `db:"取引先グループ名"`
	CreatedAt time.Time `db:"作成日時"`
	CreatedBy string    `db:"作成者名"`
	UpdatedAt time.Time `db:"更新日時"`
	UpdatedBy string    `db:"更新者名"`
}
