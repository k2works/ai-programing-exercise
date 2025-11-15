package model

import "time"

// Department は部門マスタのドメインモデルです
type Department struct {
	DepartmentCode string    `db:"部門コード"`
	StartDate      time.Time `db:"開始日"`
	EndDate        time.Time `db:"終了日"`
	DepartmentName string    `db:"部門名"`
	HierarchyLevel int       `db:"組織階層"`
	DepartmentPath string    `db:"部門パス"`
	IsLowestLevel  int       `db:"最下層区分"`
	CanEnterSlip   int       `db:"伝票入力可否"`
	CreatedAt      time.Time `db:"作成日時"`
	CreatedBy      string    `db:"作成者名"`
	UpdatedAt      time.Time `db:"更新日時"`
	UpdatedBy      string    `db:"更新者名"`
}
