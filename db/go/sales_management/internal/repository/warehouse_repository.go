package repository

import (
	"database/sql"
	"fmt"

	"github.com/jmoiron/sqlx"
	"github.com/k2works/sales-management-db/internal/model"
)

// WarehouseRepository は倉庫マスタのデータアクセスを提供します
type WarehouseRepository struct {
	db *sqlx.DB
}

// NewWarehouseRepository は新しいWarehouseRepositoryを作成します
func NewWarehouseRepository(db *sqlx.DB) *WarehouseRepository {
	return &WarehouseRepository{db: db}
}

// Create は倉庫を登録します
func (r *WarehouseRepository) Create(warehouse *model.Warehouse) error {
	query := `
		INSERT INTO 倉庫マスタ (
			倉庫コード, 倉庫名, 郵便番号, 都道府県, 住所１, 住所２,
			電話番号, fax番号, 作成日時, 作成者名, 更新日時, 更新者名
		) VALUES (
			$1, $2, $3, $4, $5, $6,
			$7, $8, $9, $10, $11, $12
		)
	`

	_, err := r.db.Exec(query,
		warehouse.WarehouseCode, warehouse.WarehouseName, warehouse.PostalCode,
		warehouse.Prefecture, warehouse.Address1, warehouse.Address2,
		warehouse.PhoneNumber, warehouse.FaxNumber,
		warehouse.CreatedAt, warehouse.CreatedBy, warehouse.UpdatedAt, warehouse.UpdatedBy)
	if err != nil {
		return fmt.Errorf("倉庫の登録に失敗しました: %w", err)
	}

	return nil
}

// FindByID は倉庫コードで倉庫を取得します
func (r *WarehouseRepository) FindByID(warehouseCode string) (*model.Warehouse, error) {
	query := `
		SELECT 倉庫コード, 倉庫名, 郵便番号, 都道府県, 住所１, 住所２,
		       電話番号, fax番号, 作成日時, 作成者名, 更新日時, 更新者名
		FROM 倉庫マスタ
		WHERE 倉庫コード = $1
	`

	var warehouse model.Warehouse
	err := r.db.Get(&warehouse, query, warehouseCode)
	if err != nil {
		if err == sql.ErrNoRows {
			return nil, nil
		}
		return nil, fmt.Errorf("倉庫の取得に失敗しました: %w", err)
	}

	return &warehouse, nil
}

// FindAll はすべての倉庫を取得します
func (r *WarehouseRepository) FindAll() ([]*model.Warehouse, error) {
	query := `
		SELECT 倉庫コード, 倉庫名, 郵便番号, 都道府県, 住所１, 住所２,
		       電話番号, fax番号, 作成日時, 作成者名, 更新日時, 更新者名
		FROM 倉庫マスタ
		ORDER BY 倉庫コード
	`

	var warehouses []*model.Warehouse
	err := r.db.Select(&warehouses, query)
	if err != nil {
		return nil, fmt.Errorf("倉庫一覧の取得に失敗しました: %w", err)
	}

	return warehouses, nil
}

// Update は倉庫を更新します
func (r *WarehouseRepository) Update(warehouse *model.Warehouse) error {
	query := `
		UPDATE 倉庫マスタ
		SET 倉庫名 = $1,
		    郵便番号 = $2,
		    都道府県 = $3,
		    住所１ = $4,
		    住所２ = $5,
		    電話番号 = $6,
		    fax番号 = $7,
		    更新日時 = $8,
		    更新者名 = $9
		WHERE 倉庫コード = $10
	`

	result, err := r.db.Exec(query,
		warehouse.WarehouseName, warehouse.PostalCode, warehouse.Prefecture,
		warehouse.Address1, warehouse.Address2,
		warehouse.PhoneNumber, warehouse.FaxNumber,
		warehouse.UpdatedAt, warehouse.UpdatedBy,
		warehouse.WarehouseCode)
	if err != nil {
		return fmt.Errorf("倉庫の更新に失敗しました: %w", err)
	}

	rowsAffected, err := result.RowsAffected()
	if err != nil {
		return fmt.Errorf("更新結果の取得に失敗しました: %w", err)
	}

	if rowsAffected == 0 {
		return fmt.Errorf("倉庫が見つかりません: %s", warehouse.WarehouseCode)
	}

	return nil
}

// Delete は倉庫を削除します
func (r *WarehouseRepository) Delete(warehouseCode string) error {
	query := `DELETE FROM 倉庫マスタ WHERE 倉庫コード = $1`

	result, err := r.db.Exec(query, warehouseCode)
	if err != nil {
		return fmt.Errorf("倉庫の削除に失敗しました: %w", err)
	}

	rowsAffected, err := result.RowsAffected()
	if err != nil {
		return fmt.Errorf("削除結果の取得に失敗しました: %w", err)
	}

	if rowsAffected == 0 {
		return fmt.Errorf("倉庫が見つかりません: %s", warehouseCode)
	}

	return nil
}
