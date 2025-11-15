package repository

import (
	"database/sql"
	"fmt"

	"github.com/jmoiron/sqlx"
	"github.com/k2works/sales-management-db/internal/model"
)

// StockRepository は在庫データのデータアクセスを提供します
type StockRepository struct {
	db *sqlx.DB
}

// NewStockRepository は新しいStockRepositoryを作成します
func NewStockRepository(db *sqlx.DB) *StockRepository {
	return &StockRepository{db: db}
}

// Create は在庫を登録します
func (r *StockRepository) Create(stock *model.Stock) error {
	query := `
		INSERT INTO 在庫 (
			倉庫コード, 商品コード, ロット番号, 在庫区分, 品質区分,
			実在庫数, 有効在庫数, 最終出荷日,
			作成日時, 作成者名, 更新日時, 更新者名
		) VALUES (
			$1, $2, $3, $4, $5,
			$6, $7, $8,
			$9, $10, $11, $12
		)
	`

	_, err := r.db.Exec(query,
		stock.WarehouseCode, stock.ProductCode, stock.LotNumber,
		stock.StockCategory, stock.QualityCategory,
		stock.ActualStock, stock.AvailableStock, stock.LastShippingDate,
		stock.CreatedAt, stock.CreatedBy, stock.UpdatedAt, stock.UpdatedBy)
	if err != nil {
		return fmt.Errorf("在庫の登録に失敗しました: %w", err)
	}

	return nil
}

// FindByID は複合主キーで在庫を取得します
func (r *StockRepository) FindByID(warehouseCode, productCode, lotNumber, stockCategory, qualityCategory string) (*model.Stock, error) {
	query := `
		SELECT 倉庫コード, 商品コード, ロット番号, 在庫区分, 品質区分,
		       実在庫数, 有効在庫数, 最終出荷日,
		       作成日時, 作成者名, 更新日時, 更新者名
		FROM 在庫
		WHERE 倉庫コード = $1
		  AND 商品コード = $2
		  AND ロット番号 = $3
		  AND 在庫区分 = $4
		  AND 品質区分 = $5
	`

	var stock model.Stock
	err := r.db.Get(&stock, query, warehouseCode, productCode, lotNumber, stockCategory, qualityCategory)
	if err != nil {
		if err == sql.ErrNoRows {
			return nil, nil
		}
		return nil, fmt.Errorf("在庫の取得に失敗しました: %w", err)
	}

	return &stock, nil
}

// FindAll はすべての在庫を取得します
func (r *StockRepository) FindAll() ([]*model.Stock, error) {
	query := `
		SELECT 倉庫コード, 商品コード, ロット番号, 在庫区分, 品質区分,
		       実在庫数, 有効在庫数, 最終出荷日,
		       作成日時, 作成者名, 更新日時, 更新者名
		FROM 在庫
		ORDER BY 倉庫コード, 商品コード, ロット番号, 在庫区分, 品質区分
	`

	var stocks []*model.Stock
	err := r.db.Select(&stocks, query)
	if err != nil {
		return nil, fmt.Errorf("在庫一覧の取得に失敗しました: %w", err)
	}

	return stocks, nil
}

// FindByWarehouse は指定された倉庫の在庫を取得します
func (r *StockRepository) FindByWarehouse(warehouseCode string) ([]*model.Stock, error) {
	query := `
		SELECT 倉庫コード, 商品コード, ロット番号, 在庫区分, 品質区分,
		       実在庫数, 有効在庫数, 最終出荷日,
		       作成日時, 作成者名, 更新日時, 更新者名
		FROM 在庫
		WHERE 倉庫コード = $1
		ORDER BY 商品コード, ロット番号, 在庫区分, 品質区分
	`

	var stocks []*model.Stock
	err := r.db.Select(&stocks, query, warehouseCode)
	if err != nil {
		return nil, fmt.Errorf("倉庫別在庫の取得に失敗しました: %w", err)
	}

	return stocks, nil
}

// FindByProduct は指定された商品の在庫を取得します
func (r *StockRepository) FindByProduct(productCode string) ([]*model.Stock, error) {
	query := `
		SELECT 倉庫コード, 商品コード, ロット番号, 在庫区分, 品質区分,
		       実在庫数, 有効在庫数, 最終出荷日,
		       作成日時, 作成者名, 更新日時, 更新者名
		FROM 在庫
		WHERE 商品コード = $1
		ORDER BY 倉庫コード, ロット番号, 在庫区分, 品質区分
	`

	var stocks []*model.Stock
	err := r.db.Select(&stocks, query, productCode)
	if err != nil {
		return nil, fmt.Errorf("商品別在庫の取得に失敗しました: %w", err)
	}

	return stocks, nil
}

// Update は在庫を更新します
func (r *StockRepository) Update(stock *model.Stock) error {
	query := `
		UPDATE 在庫
		SET 実在庫数 = $1,
		    有効在庫数 = $2,
		    最終出荷日 = $3,
		    更新日時 = $4,
		    更新者名 = $5
		WHERE 倉庫コード = $6
		  AND 商品コード = $7
		  AND ロット番号 = $8
		  AND 在庫区分 = $9
		  AND 品質区分 = $10
	`

	result, err := r.db.Exec(query,
		stock.ActualStock, stock.AvailableStock, stock.LastShippingDate,
		stock.UpdatedAt, stock.UpdatedBy,
		stock.WarehouseCode, stock.ProductCode, stock.LotNumber,
		stock.StockCategory, stock.QualityCategory)
	if err != nil {
		return fmt.Errorf("在庫の更新に失敗しました: %w", err)
	}

	rowsAffected, err := result.RowsAffected()
	if err != nil {
		return fmt.Errorf("更新結果の取得に失敗しました: %w", err)
	}

	if rowsAffected == 0 {
		return fmt.Errorf("在庫が見つかりません")
	}

	return nil
}

// Delete は在庫を削除します
func (r *StockRepository) Delete(warehouseCode, productCode, lotNumber, stockCategory, qualityCategory string) error {
	query := `
		DELETE FROM 在庫
		WHERE 倉庫コード = $1
		  AND 商品コード = $2
		  AND ロット番号 = $3
		  AND 在庫区分 = $4
		  AND 品質区分 = $5
	`

	result, err := r.db.Exec(query, warehouseCode, productCode, lotNumber, stockCategory, qualityCategory)
	if err != nil {
		return fmt.Errorf("在庫の削除に失敗しました: %w", err)
	}

	rowsAffected, err := result.RowsAffected()
	if err != nil {
		return fmt.Errorf("削除結果の取得に失敗しました: %w", err)
	}

	if rowsAffected == 0 {
		return fmt.Errorf("在庫が見つかりません")
	}

	return nil
}
