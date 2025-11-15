package repository

import (
	"database/sql"
	"fmt"

	"github.com/jmoiron/sqlx"
	"github.com/k2works/sales-management-db/internal/model"
)

// EmployeeRepository は社員マスタのデータアクセスを提供します
type EmployeeRepository struct {
	db *sqlx.DB
}

// NewEmployeeRepository は新しいEmployeeRepositoryを作成します
func NewEmployeeRepository(db *sqlx.DB) *EmployeeRepository {
	return &EmployeeRepository{db: db}
}

// Create は社員を登録します
func (r *EmployeeRepository) Create(emp *model.Employee) error {
	query := `
		INSERT INTO 社員マスタ (
			社員コード, 社員名, 社員名カナ, パスワード,
			電話番号, FAX番号, 部門コード, 開始日,
			職種コード, 承認権限コード,
			作成日時, 作成者名, 更新日時, 更新者名
		) VALUES (
			$1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14
		)
	`

	_, err := r.db.Exec(query,
		emp.EmployeeCode, emp.EmployeeName, emp.EmployeeNameKana, emp.Password,
		emp.PhoneNumber, emp.FaxNumber, emp.DepartmentCode, emp.StartDate,
		emp.JobTypeCode, emp.ApprovalAuthorityCode,
		emp.CreatedAt, emp.CreatedBy, emp.UpdatedAt, emp.UpdatedBy)
	if err != nil {
		return fmt.Errorf("社員の登録に失敗しました: %w", err)
	}

	return nil
}

// FindByID は社員コードで社員を取得します
func (r *EmployeeRepository) FindByID(empCode string) (*model.Employee, error) {
	query := `
		SELECT 社員コード, 社員名, 社員名カナ, パスワード, 電話番号, FAX番号,
		       部門コード, 開始日, 職種コード, 承認権限コード,
		       作成日時, 作成者名, 更新日時, 更新者名
		FROM 社員マスタ
		WHERE 社員コード = $1
	`

	var emp model.Employee
	err := r.db.Get(&emp, query, empCode)
	if err != nil {
		if err == sql.ErrNoRows {
			return nil, nil
		}
		return nil, fmt.Errorf("社員の取得に失敗しました: %w", err)
	}

	return &emp, nil
}

// FindAll はすべての社員を取得します
func (r *EmployeeRepository) FindAll() ([]*model.Employee, error) {
	query := `
		SELECT 社員コード, 社員名, 社員名カナ, パスワード, 電話番号, FAX番号,
		       部門コード, 開始日, 職種コード, 承認権限コード,
		       作成日時, 作成者名, 更新日時, 更新者名
		FROM 社員マスタ
		ORDER BY 社員コード
	`

	var emps []*model.Employee
	err := r.db.Select(&emps, query)
	if err != nil {
		return nil, fmt.Errorf("社員一覧の取得に失敗しました: %w", err)
	}

	return emps, nil
}

// FindByDepartment は部門コードで社員を検索します
func (r *EmployeeRepository) FindByDepartment(deptCode string) ([]*model.Employee, error) {
	query := `
		SELECT 社員コード, 社員名, 社員名カナ, パスワード, 電話番号, FAX番号,
		       部門コード, 開始日, 職種コード, 承認権限コード,
		       作成日時, 作成者名, 更新日時, 更新者名
		FROM 社員マスタ
		WHERE 部門コード = $1
		ORDER BY 社員コード
	`

	var emps []*model.Employee
	err := r.db.Select(&emps, query, deptCode)
	if err != nil {
		return nil, fmt.Errorf("部門別社員の取得に失敗しました: %w", err)
	}

	return emps, nil
}

// Update は社員を更新します
func (r *EmployeeRepository) Update(emp *model.Employee) error {
	query := `
		UPDATE 社員マスタ
		SET 社員名 = $1,
		    社員名カナ = $2,
		    パスワード = $3,
		    電話番号 = $4,
		    FAX番号 = $5,
		    部門コード = $6,
		    開始日 = $7,
		    職種コード = $8,
		    承認権限コード = $9,
		    更新日時 = $10,
		    更新者名 = $11
		WHERE 社員コード = $12
	`

	result, err := r.db.Exec(query,
		emp.EmployeeName, emp.EmployeeNameKana, emp.Password,
		emp.PhoneNumber, emp.FaxNumber, emp.DepartmentCode, emp.StartDate,
		emp.JobTypeCode, emp.ApprovalAuthorityCode,
		emp.UpdatedAt, emp.UpdatedBy, emp.EmployeeCode)
	if err != nil {
		return fmt.Errorf("社員の更新に失敗しました: %w", err)
	}

	rowsAffected, err := result.RowsAffected()
	if err != nil {
		return fmt.Errorf("更新結果の取得に失敗しました: %w", err)
	}

	if rowsAffected == 0 {
		return fmt.Errorf("社員が見つかりません: %s", emp.EmployeeCode)
	}

	return nil
}

// Delete は社員を削除します
func (r *EmployeeRepository) Delete(empCode string) error {
	query := `
		DELETE FROM 社員マスタ
		WHERE 社員コード = $1
	`

	result, err := r.db.Exec(query, empCode)
	if err != nil {
		return fmt.Errorf("社員の削除に失敗しました: %w", err)
	}

	rowsAffected, err := result.RowsAffected()
	if err != nil {
		return fmt.Errorf("削除結果の取得に失敗しました: %w", err)
	}

	if rowsAffected == 0 {
		return fmt.Errorf("社員が見つかりません: %s", empCode)
	}

	return nil
}
