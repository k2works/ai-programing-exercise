package repository

import (
	"database/sql"
	"fmt"

	"github.com/jmoiron/sqlx"
	"github.com/k2works/sales-management-db/internal/model"
)

// ProductRepository は商品マスタのデータアクセスを提供します
type ProductRepository struct {
	db *sqlx.DB
}

// NewProductRepository は新しいProductRepositoryを作成します
func NewProductRepository(db *sqlx.DB) *ProductRepository {
	return &ProductRepository{db: db}
}

// Create は商品を登録します
func (r *ProductRepository) Create(product *model.Product) error {
	query := `
		INSERT INTO 商品マスタ (
			商品コード, 商品正式名, 商品略称, 商品名カナ, 商品区分, 製品型番,
			販売単価, 仕入単価, 売上原価, 税区分,
			商品分類コード, 雑区分, 在庫管理対象区分, 在庫引当区分,
			仕入先コード, 仕入先枝番,
			作成日時, 作成者名, 更新日時, 更新者名
		) VALUES (
			$1, $2, $3, $4, $5, $6,
			$7, $8, $9, $10,
			$11, $12, $13, $14,
			$15, $16,
			$17, $18, $19, $20
		)
	`

	_, err := r.db.Exec(query,
		product.ProductCode, product.ProductFullName, product.ProductAbbreviation,
		product.ProductNameKana, product.ProductType, product.ModelNumber,
		product.SellingPrice, product.PurchasePrice, product.CostOfSales, product.TaxCategory,
		product.ProductCategoryCode, product.MiscCategory, product.InventoryManaged, product.StockAllocation,
		product.SupplierCode, product.SupplierBranch,
		product.CreatedAt, product.CreatedBy, product.UpdatedAt, product.UpdatedBy)
	if err != nil {
		return fmt.Errorf("商品の登録に失敗しました: %w", err)
	}

	return nil
}

// FindByID は商品コードで商品を取得します
func (r *ProductRepository) FindByID(prodCode string) (*model.Product, error) {
	query := `
		SELECT 商品コード, 商品正式名, 商品略称, 商品名カナ, 商品区分, 製品型番,
		       販売単価, 仕入単価, 売上原価, 税区分,
		       商品分類コード, 雑区分, 在庫管理対象区分, 在庫引当区分,
		       仕入先コード, 仕入先枝番,
		       作成日時, 作成者名, 更新日時, 更新者名
		FROM 商品マスタ
		WHERE 商品コード = $1
	`

	var product model.Product
	err := r.db.Get(&product, query, prodCode)
	if err != nil {
		if err == sql.ErrNoRows {
			return nil, nil
		}
		return nil, fmt.Errorf("商品の取得に失敗しました: %w", err)
	}

	return &product, nil
}

// FindAll はすべての商品を取得します
func (r *ProductRepository) FindAll() ([]*model.Product, error) {
	query := `
		SELECT 商品コード, 商品正式名, 商品略称, 商品名カナ, 商品区分, 製品型番,
		       販売単価, 仕入単価, 売上原価, 税区分,
		       商品分類コード, 雑区分, 在庫管理対象区分, 在庫引当区分,
		       仕入先コード, 仕入先枝番,
		       作成日時, 作成者名, 更新日時, 更新者名
		FROM 商品マスタ
		ORDER BY 商品コード
	`

	var products []*model.Product
	err := r.db.Select(&products, query)
	if err != nil {
		return nil, fmt.Errorf("商品一覧の取得に失敗しました: %w", err)
	}

	return products, nil
}

// Update は商品を更新します
func (r *ProductRepository) Update(product *model.Product) error {
	query := `
		UPDATE 商品マスタ
		SET 商品正式名 = $1,
		    商品略称 = $2,
		    商品名カナ = $3,
		    商品区分 = $4,
		    製品型番 = $5,
		    販売単価 = $6,
		    仕入単価 = $7,
		    売上原価 = $8,
		    税区分 = $9,
		    商品分類コード = $10,
		    雑区分 = $11,
		    在庫管理対象区分 = $12,
		    在庫引当区分 = $13,
		    仕入先コード = $14,
		    仕入先枝番 = $15,
		    更新日時 = $16,
		    更新者名 = $17
		WHERE 商品コード = $18
	`

	result, err := r.db.Exec(query,
		product.ProductFullName, product.ProductAbbreviation, product.ProductNameKana,
		product.ProductType, product.ModelNumber,
		product.SellingPrice, product.PurchasePrice, product.CostOfSales, product.TaxCategory,
		product.ProductCategoryCode, product.MiscCategory, product.InventoryManaged, product.StockAllocation,
		product.SupplierCode, product.SupplierBranch,
		product.UpdatedAt, product.UpdatedBy,
		product.ProductCode)
	if err != nil {
		return fmt.Errorf("商品の更新に失敗しました: %w", err)
	}

	rows, err := result.RowsAffected()
	if err != nil {
		return fmt.Errorf("更新結果の取得に失敗しました: %w", err)
	}

	if rows == 0 {
		return fmt.Errorf("商品が見つかりません: %s", product.ProductCode)
	}

	return nil
}

// Delete は商品を削除します
func (r *ProductRepository) Delete(prodCode string) error {
	query := `DELETE FROM 商品マスタ WHERE 商品コード = $1`

	result, err := r.db.Exec(query, prodCode)
	if err != nil {
		return fmt.Errorf("商品の削除に失敗しました: %w", err)
	}

	rows, err := result.RowsAffected()
	if err != nil {
		return fmt.Errorf("削除結果の取得に失敗しました: %w", err)
	}

	if rows == 0 {
		return fmt.Errorf("商品が見つかりません: %s", prodCode)
	}

	return nil
}
