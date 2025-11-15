package repository

import (
	"database/sql"
	"fmt"

	"github.com/jmoiron/sqlx"
	"github.com/k2works/sales-management-db/internal/model"
)

// AlternateProductRepository は代替商品のデータアクセスを提供します
type AlternateProductRepository struct {
	db *sqlx.DB
}

// NewAlternateProductRepository は新しいAlternateProductRepositoryを作成します
func NewAlternateProductRepository(db *sqlx.DB) *AlternateProductRepository {
	return &AlternateProductRepository{db: db}
}

// Create は代替商品を登録します
func (r *AlternateProductRepository) Create(alternate *model.AlternateProduct) error {
	query := `
		INSERT INTO 代替商品 (
			商品コード, 代替商品コード, 優先順位,
			作成日時, 作成者名, 更新日時, 更新者名
		) VALUES (
			$1, $2, $3,
			$4, $5, $6, $7
		)
	`

	_, err := r.db.Exec(query,
		alternate.ProductCode, alternate.AlternateProductCode, alternate.Priority,
		alternate.CreatedAt, alternate.CreatedBy, alternate.UpdatedAt, alternate.UpdatedBy)
	if err != nil {
		return fmt.Errorf("代替商品の登録に失敗しました: %w", err)
	}

	return nil
}

// FindByID は商品コードと代替商品コードで代替商品を取得します
func (r *AlternateProductRepository) FindByID(productCode, alternateCode string) (*model.AlternateProduct, error) {
	query := `
		SELECT 商品コード, 代替商品コード, 優先順位,
		       作成日時, 作成者名, 更新日時, 更新者名
		FROM 代替商品
		WHERE 商品コード = $1 AND 代替商品コード = $2
	`

	var alternate model.AlternateProduct
	err := r.db.Get(&alternate, query, productCode, alternateCode)
	if err != nil {
		if err == sql.ErrNoRows {
			return nil, nil
		}
		return nil, fmt.Errorf("代替商品の取得に失敗しました: %w", err)
	}

	return &alternate, nil
}

// FindByProduct は商品コードで代替商品を取得します（優先順位順）
func (r *AlternateProductRepository) FindByProduct(productCode string) ([]*model.AlternateProduct, error) {
	query := `
		SELECT 商品コード, 代替商品コード, 優先順位,
		       作成日時, 作成者名, 更新日時, 更新者名
		FROM 代替商品
		WHERE 商品コード = $1
		ORDER BY 優先順位
	`

	var alternates []*model.AlternateProduct
	err := r.db.Select(&alternates, query, productCode)
	if err != nil {
		return nil, fmt.Errorf("商品別の代替商品取得に失敗しました: %w", err)
	}

	return alternates, nil
}

// Update は代替商品を更新します
func (r *AlternateProductRepository) Update(alternate *model.AlternateProduct) error {
	query := `
		UPDATE 代替商品
		SET 優先順位 = $1,
		    更新日時 = $2,
		    更新者名 = $3
		WHERE 商品コード = $4 AND 代替商品コード = $5
	`

	result, err := r.db.Exec(query,
		alternate.Priority,
		alternate.UpdatedAt, alternate.UpdatedBy,
		alternate.ProductCode, alternate.AlternateProductCode)
	if err != nil {
		return fmt.Errorf("代替商品の更新に失敗しました: %w", err)
	}

	rows, err := result.RowsAffected()
	if err != nil {
		return fmt.Errorf("更新結果の取得に失敗しました: %w", err)
	}

	if rows == 0 {
		return fmt.Errorf("代替商品が見つかりません: 商品=%s, 代替商品=%s", alternate.ProductCode, alternate.AlternateProductCode)
	}

	return nil
}

// Delete は代替商品を削除します
func (r *AlternateProductRepository) Delete(productCode, alternateCode string) error {
	query := `DELETE FROM 代替商品 WHERE 商品コード = $1 AND 代替商品コード = $2`

	result, err := r.db.Exec(query, productCode, alternateCode)
	if err != nil {
		return fmt.Errorf("代替商品の削除に失敗しました: %w", err)
	}

	rows, err := result.RowsAffected()
	if err != nil {
		return fmt.Errorf("削除結果の取得に失敗しました: %w", err)
	}

	if rows == 0 {
		return fmt.Errorf("代替商品が見つかりません: 商品=%s, 代替商品=%s", productCode, alternateCode)
	}

	return nil
}
