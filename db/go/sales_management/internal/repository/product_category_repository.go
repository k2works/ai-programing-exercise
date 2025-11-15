package repository

import (
	"database/sql"
	"fmt"

	"github.com/jmoiron/sqlx"
	"github.com/k2works/sales-management-db/internal/model"
)

// ProductCategoryRepository は商品分類マスタのデータアクセスを提供します
type ProductCategoryRepository struct {
	db *sqlx.DB
}

// NewProductCategoryRepository は新しいProductCategoryRepositoryを作成します
func NewProductCategoryRepository(db *sqlx.DB) *ProductCategoryRepository {
	return &ProductCategoryRepository{db: db}
}

// Create は商品分類を登録します
func (r *ProductCategoryRepository) Create(category *model.ProductCategory) error {
	query := `
		INSERT INTO 商品分類マスタ (
			商品分類コード, 商品分類名, 商品分類階層, 商品分類パス, 最下層区分,
			作成日時, 作成者名, 更新日時, 更新者名
		) VALUES (
			$1, $2, $3, $4, $5,
			$6, $7, $8, $9
		)
	`

	_, err := r.db.Exec(query,
		category.CategoryCode, category.CategoryName, category.CategoryLevel,
		category.CategoryPath, category.IsLowestLevel,
		category.CreatedAt, category.CreatedBy, category.UpdatedAt, category.UpdatedBy)
	if err != nil {
		return fmt.Errorf("商品分類の登録に失敗しました: %w", err)
	}

	return nil
}

// FindByID は商品分類コードで商品分類を取得します
func (r *ProductCategoryRepository) FindByID(categoryCode string) (*model.ProductCategory, error) {
	query := `
		SELECT 商品分類コード, 商品分類名, 商品分類階層, 商品分類パス, 最下層区分,
		       作成日時, 作成者名, 更新日時, 更新者名
		FROM 商品分類マスタ
		WHERE 商品分類コード = $1
	`

	var category model.ProductCategory
	err := r.db.Get(&category, query, categoryCode)
	if err != nil {
		if err == sql.ErrNoRows {
			return nil, nil
		}
		return nil, fmt.Errorf("商品分類の取得に失敗しました: %w", err)
	}

	return &category, nil
}

// FindAll はすべての商品分類を取得します
func (r *ProductCategoryRepository) FindAll() ([]*model.ProductCategory, error) {
	query := `
		SELECT 商品分類コード, 商品分類名, 商品分類階層, 商品分類パス, 最下層区分,
		       作成日時, 作成者名, 更新日時, 更新者名
		FROM 商品分類マスタ
		ORDER BY 商品分類パス
	`

	var categories []*model.ProductCategory
	err := r.db.Select(&categories, query)
	if err != nil {
		return nil, fmt.Errorf("商品分類一覧の取得に失敗しました: %w", err)
	}

	return categories, nil
}

// FindByLevel は階層レベルで商品分類を取得します
func (r *ProductCategoryRepository) FindByLevel(level int) ([]*model.ProductCategory, error) {
	query := `
		SELECT 商品分類コード, 商品分類名, 商品分類階層, 商品分類パス, 最下層区分,
		       作成日時, 作成者名, 更新日時, 更新者名
		FROM 商品分類マスタ
		WHERE 商品分類階層 = $1
		ORDER BY 商品分類パス
	`

	var categories []*model.ProductCategory
	err := r.db.Select(&categories, query, level)
	if err != nil {
		return nil, fmt.Errorf("階層レベルによる商品分類取得に失敗しました: %w", err)
	}

	return categories, nil
}

// Update は商品分類を更新します
func (r *ProductCategoryRepository) Update(category *model.ProductCategory) error {
	query := `
		UPDATE 商品分類マスタ
		SET 商品分類名 = $1,
		    商品分類階層 = $2,
		    商品分類パス = $3,
		    最下層区分 = $4,
		    更新日時 = $5,
		    更新者名 = $6
		WHERE 商品分類コード = $7
	`

	result, err := r.db.Exec(query,
		category.CategoryName, category.CategoryLevel,
		category.CategoryPath, category.IsLowestLevel,
		category.UpdatedAt, category.UpdatedBy,
		category.CategoryCode)
	if err != nil {
		return fmt.Errorf("商品分類の更新に失敗しました: %w", err)
	}

	rows, err := result.RowsAffected()
	if err != nil {
		return fmt.Errorf("更新結果の取得に失敗しました: %w", err)
	}

	if rows == 0 {
		return fmt.Errorf("商品分類が見つかりません: %s", category.CategoryCode)
	}

	return nil
}

// Delete は商品分類を削除します
func (r *ProductCategoryRepository) Delete(categoryCode string) error {
	query := `DELETE FROM 商品分類マスタ WHERE 商品分類コード = $1`

	result, err := r.db.Exec(query, categoryCode)
	if err != nil {
		return fmt.Errorf("商品分類の削除に失敗しました: %w", err)
	}

	rows, err := result.RowsAffected()
	if err != nil {
		return fmt.Errorf("削除結果の取得に失敗しました: %w", err)
	}

	if rows == 0 {
		return fmt.Errorf("商品分類が見つかりません: %s", categoryCode)
	}

	return nil
}
