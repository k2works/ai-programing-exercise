package repository

import (
	"database/sql"
	"fmt"

	"github.com/jmoiron/sqlx"
	"github.com/k2works/sales-management-db/internal/model"
)

// PriceByCustomerRepository は顧客別販売単価のデータアクセスを提供します
type PriceByCustomerRepository struct {
	db *sqlx.DB
}

// NewPriceByCustomerRepository は新しいPriceByCustomerRepositoryを作成します
func NewPriceByCustomerRepository(db *sqlx.DB) *PriceByCustomerRepository {
	return &PriceByCustomerRepository{db: db}
}

// Create は顧客別単価を登録します
func (r *PriceByCustomerRepository) Create(price *model.PriceByCustomer) error {
	query := `
		INSERT INTO 顧客別販売単価 (
			商品コード, 取引先コード, 販売単価,
			作成日時, 作成者名, 更新日時, 更新者名
		) VALUES (
			$1, $2, $3,
			$4, $5, $6, $7
		)
	`

	_, err := r.db.Exec(query,
		price.ProductCode, price.CustomerCode, price.Price,
		price.CreatedAt, price.CreatedBy, price.UpdatedAt, price.UpdatedBy)
	if err != nil {
		return fmt.Errorf("顧客別単価の登録に失敗しました: %w", err)
	}

	return nil
}

// FindByID は商品コードと取引先コードで顧客別単価を取得します
func (r *PriceByCustomerRepository) FindByID(productCode, customerCode string) (*model.PriceByCustomer, error) {
	query := `
		SELECT 商品コード, 取引先コード, 販売単価,
		       作成日時, 作成者名, 更新日時, 更新者名
		FROM 顧客別販売単価
		WHERE 商品コード = $1 AND 取引先コード = $2
	`

	var price model.PriceByCustomer
	err := r.db.Get(&price, query, productCode, customerCode)
	if err != nil {
		if err == sql.ErrNoRows {
			return nil, nil
		}
		return nil, fmt.Errorf("顧客別単価の取得に失敗しました: %w", err)
	}

	return &price, nil
}

// FindByProduct は商品コードで顧客別単価を取得します
func (r *PriceByCustomerRepository) FindByProduct(productCode string) ([]*model.PriceByCustomer, error) {
	query := `
		SELECT 商品コード, 取引先コード, 販売単価,
		       作成日時, 作成者名, 更新日時, 更新者名
		FROM 顧客別販売単価
		WHERE 商品コード = $1
		ORDER BY 取引先コード
	`

	var prices []*model.PriceByCustomer
	err := r.db.Select(&prices, query, productCode)
	if err != nil {
		return nil, fmt.Errorf("商品別の顧客別単価取得に失敗しました: %w", err)
	}

	return prices, nil
}

// Update は顧客別単価を更新します
func (r *PriceByCustomerRepository) Update(price *model.PriceByCustomer) error {
	query := `
		UPDATE 顧客別販売単価
		SET 販売単価 = $1,
		    更新日時 = $2,
		    更新者名 = $3
		WHERE 商品コード = $4 AND 取引先コード = $5
	`

	result, err := r.db.Exec(query,
		price.Price,
		price.UpdatedAt, price.UpdatedBy,
		price.ProductCode, price.CustomerCode)
	if err != nil {
		return fmt.Errorf("顧客別単価の更新に失敗しました: %w", err)
	}

	rows, err := result.RowsAffected()
	if err != nil {
		return fmt.Errorf("更新結果の取得に失敗しました: %w", err)
	}

	if rows == 0 {
		return fmt.Errorf("顧客別単価が見つかりません: 商品=%s, 顧客=%s", price.ProductCode, price.CustomerCode)
	}

	return nil
}

// Delete は顧客別単価を削除します
func (r *PriceByCustomerRepository) Delete(productCode, customerCode string) error {
	query := `DELETE FROM 顧客別販売単価 WHERE 商品コード = $1 AND 取引先コード = $2`

	result, err := r.db.Exec(query, productCode, customerCode)
	if err != nil {
		return fmt.Errorf("顧客別単価の削除に失敗しました: %w", err)
	}

	rows, err := result.RowsAffected()
	if err != nil {
		return fmt.Errorf("削除結果の取得に失敗しました: %w", err)
	}

	if rows == 0 {
		return fmt.Errorf("顧客別単価が見つかりません: 商品=%s, 顧客=%s", productCode, customerCode)
	}

	return nil
}
