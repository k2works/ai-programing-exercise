package model

import "time"

// Employee は社員マスタのドメインモデルです
type Employee struct {
	EmployeeCode          string    `db:"社員コード"`
	EmployeeName          string    `db:"社員名"`
	EmployeeNameKana      string    `db:"社員名カナ"`
	Password              string    `db:"パスワード"`
	PhoneNumber           string    `db:"電話番号"`
	FaxNumber             string    `db:"fax番号"`
	DepartmentCode        string    `db:"部門コード"` // 外部キー
	StartDate             time.Time `db:"開始日"`
	JobTypeCode           string    `db:"職種コード"`
	ApprovalAuthorityCode string    `db:"承認権限コード"`
	CreatedAt             time.Time `db:"作成日時"`
	CreatedBy             string    `db:"作成者名"`
	UpdatedAt             time.Time `db:"更新日時"`
	UpdatedBy             string    `db:"更新者名"`
}
