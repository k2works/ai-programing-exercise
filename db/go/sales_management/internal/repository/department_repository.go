package repository

import (
	"database/sql"
	"fmt"
	"time"

	"github.com/jmoiron/sqlx"
	"github.com/k2works/sales-management-db/internal/model"
)

// DepartmentRepository は部門マスタのデータアクセスを提供します
type DepartmentRepository struct {
	db *sqlx.DB
}

// NewDepartmentRepository は新しいDepartmentRepositoryを作成します
func NewDepartmentRepository(db *sqlx.DB) *DepartmentRepository {
	return &DepartmentRepository{db: db}
}

// Create は部門を登録します
func (r *DepartmentRepository) Create(dept *model.Department) error {
	query := `
		INSERT INTO 部門マスタ (
			部門コード, 開始日, 終了日, 部門名, 組織階層, 部門パス,
			最下層区分, 伝票入力可否, 作成日時, 作成者名, 更新日時, 更新者名
		) VALUES (
			$1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12
		)
	`

	_, err := r.db.Exec(query,
		dept.DepartmentCode, dept.StartDate, dept.EndDate, dept.DepartmentName,
		dept.HierarchyLevel, dept.DepartmentPath, dept.IsLowestLevel, dept.CanEnterSlip,
		dept.CreatedAt, dept.CreatedBy, dept.UpdatedAt, dept.UpdatedBy)
	if err != nil {
		return fmt.Errorf("部門の登録に失敗しました: %w", err)
	}

	return nil
}

// FindByID は部門コードと開始日で部門を取得します
func (r *DepartmentRepository) FindByID(deptCode string, startDate time.Time) (*model.Department, error) {
	query := `
		SELECT 部門コード, 開始日, 終了日, 部門名, 組織階層, 部門パス,
		       最下層区分, 伝票入力可否, 作成日時, 作成者名, 更新日時, 更新者名
		FROM 部門マスタ
		WHERE 部門コード = $1 AND 開始日 = $2
	`

	var dept model.Department
	err := r.db.Get(&dept, query, deptCode, startDate)
	if err != nil {
		if err == sql.ErrNoRows {
			return nil, nil
		}
		return nil, fmt.Errorf("部門の取得に失敗しました: %w", err)
	}

	return &dept, nil
}

// FindAll はすべての部門を取得します
func (r *DepartmentRepository) FindAll() ([]*model.Department, error) {
	query := `
		SELECT 部門コード, 開始日, 終了日, 部門名, 組織階層, 部門パス,
		       最下層区分, 伝票入力可否, 作成日時, 作成者名, 更新日時, 更新者名
		FROM 部門マスタ
		ORDER BY 部門コード, 開始日
	`

	var depts []*model.Department
	err := r.db.Select(&depts, query)
	if err != nil {
		return nil, fmt.Errorf("部門一覧の取得に失敗しました: %w", err)
	}

	return depts, nil
}

// Update は部門を更新します
func (r *DepartmentRepository) Update(dept *model.Department) error {
	query := `
		UPDATE 部門マスタ
		SET 部門名 = $1,
		    組織階層 = $2,
		    部門パス = $3,
		    最下層区分 = $4,
		    伝票入力可否 = $5,
		    終了日 = $6,
		    更新日時 = $7,
		    更新者名 = $8
		WHERE 部門コード = $9 AND 開始日 = $10
	`

	result, err := r.db.Exec(query,
		dept.DepartmentName, dept.HierarchyLevel, dept.DepartmentPath,
		dept.IsLowestLevel, dept.CanEnterSlip, dept.EndDate,
		dept.UpdatedAt, dept.UpdatedBy, dept.DepartmentCode, dept.StartDate)
	if err != nil {
		return fmt.Errorf("部門の更新に失敗しました: %w", err)
	}

	rowsAffected, err := result.RowsAffected()
	if err != nil {
		return fmt.Errorf("更新結果の取得に失敗しました: %w", err)
	}

	if rowsAffected == 0 {
		return fmt.Errorf("部門が見つかりません: %s", dept.DepartmentCode)
	}

	return nil
}

// Delete は部門を削除します
func (r *DepartmentRepository) Delete(deptCode string, startDate time.Time) error {
	query := `
		DELETE FROM 部門マスタ
		WHERE 部門コード = $1 AND 開始日 = $2
	`

	result, err := r.db.Exec(query, deptCode, startDate)
	if err != nil {
		return fmt.Errorf("部門の削除に失敗しました: %w", err)
	}

	rowsAffected, err := result.RowsAffected()
	if err != nil {
		return fmt.Errorf("削除結果の取得に失敗しました: %w", err)
	}

	if rowsAffected == 0 {
		return fmt.Errorf("部門が見つかりません: %s", deptCode)
	}

	return nil
}
