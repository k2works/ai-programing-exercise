package repository

import (
	"database/sql"
	"fmt"

	"github.com/jmoiron/sqlx"
	"github.com/k2works/sales-management-db/internal/model"
)

// CompanyGroupRepository は取引先グループマスタのデータアクセスを提供します
type CompanyGroupRepository struct {
	db *sqlx.DB
}

// NewCompanyGroupRepository は新しいCompanyGroupRepositoryを作成します
func NewCompanyGroupRepository(db *sqlx.DB) *CompanyGroupRepository {
	return &CompanyGroupRepository{db: db}
}

// Create は取引先グループを登録します
func (r *CompanyGroupRepository) Create(group *model.CompanyGroup) error {
	query := `
		INSERT INTO 取引先グループマスタ (
			取引先グループコード, 取引先グループ名,
			作成日時, 作成者名, 更新日時, 更新者名
		) VALUES (
			$1, $2,
			$3, $4, $5, $6
		)
	`

	_, err := r.db.Exec(query,
		group.GroupCode, group.GroupName,
		group.CreatedAt, group.CreatedBy, group.UpdatedAt, group.UpdatedBy)
	if err != nil {
		return fmt.Errorf("取引先グループの登録に失敗しました: %w", err)
	}

	return nil
}

// FindByID は取引先グループコードで取引先グループを取得します
func (r *CompanyGroupRepository) FindByID(groupCode string) (*model.CompanyGroup, error) {
	query := `
		SELECT 取引先グループコード, 取引先グループ名,
		       作成日時, 作成者名, 更新日時, 更新者名
		FROM 取引先グループマスタ
		WHERE 取引先グループコード = $1
	`

	var group model.CompanyGroup
	err := r.db.Get(&group, query, groupCode)
	if err != nil {
		if err == sql.ErrNoRows {
			return nil, nil
		}
		return nil, fmt.Errorf("取引先グループの取得に失敗しました: %w", err)
	}

	return &group, nil
}

// FindAll はすべての取引先グループを取得します
func (r *CompanyGroupRepository) FindAll() ([]*model.CompanyGroup, error) {
	query := `
		SELECT 取引先グループコード, 取引先グループ名,
		       作成日時, 作成者名, 更新日時, 更新者名
		FROM 取引先グループマスタ
		ORDER BY 取引先グループコード
	`

	var groups []*model.CompanyGroup
	err := r.db.Select(&groups, query)
	if err != nil {
		return nil, fmt.Errorf("取引先グループ一覧の取得に失敗しました: %w", err)
	}

	return groups, nil
}

// Update は取引先グループを更新します
func (r *CompanyGroupRepository) Update(group *model.CompanyGroup) error {
	query := `
		UPDATE 取引先グループマスタ
		SET 取引先グループ名 = $1,
		    更新日時 = $2,
		    更新者名 = $3
		WHERE 取引先グループコード = $4
	`

	result, err := r.db.Exec(query,
		group.GroupName,
		group.UpdatedAt, group.UpdatedBy,
		group.GroupCode)
	if err != nil {
		return fmt.Errorf("取引先グループの更新に失敗しました: %w", err)
	}

	rows, err := result.RowsAffected()
	if err != nil {
		return fmt.Errorf("更新結果の取得に失敗しました: %w", err)
	}

	if rows == 0 {
		return fmt.Errorf("取引先グループが見つかりません: %s", group.GroupCode)
	}

	return nil
}

// Delete は取引先グループを削除します
func (r *CompanyGroupRepository) Delete(groupCode string) error {
	query := `DELETE FROM 取引先グループマスタ WHERE 取引先グループコード = $1`

	result, err := r.db.Exec(query, groupCode)
	if err != nil {
		return fmt.Errorf("取引先グループの削除に失敗しました: %w", err)
	}

	rows, err := result.RowsAffected()
	if err != nil {
		return fmt.Errorf("削除結果の取得に失敗しました: %w", err)
	}

	if rows == 0 {
		return fmt.Errorf("取引先グループが見つかりません: %s", groupCode)
	}

	return nil
}
