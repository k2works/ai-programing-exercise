package repository

import (
	"database/sql"
	"fmt"

	"github.com/jmoiron/sqlx"
	"github.com/k2works/sales-management-db/internal/model"
)

// SalesRepository は売上データのデータアクセスを提供します
type SalesRepository struct {
	db *sqlx.DB
}

// NewSalesRepository は新しいSalesRepositoryを作成します
func NewSalesRepository(db *sqlx.DB) *SalesRepository {
	return &SalesRepository{db: db}
}

// Create は売上を登録します
func (r *SalesRepository) Create(sales *model.Sales) error {
	query := `
		INSERT INTO 売上 (
			売上番号, 売上日, 売上区分, 受注番号,
			部門コード, 開始日, 取引先コード, 売上金額, 消費税,
			訂正番号, 元伝票番号,
			作成日時, 作成者名, 更新日時, 更新者名
		) VALUES (
			$1, $2, $3, $4,
			$5, $6, $7, $8, $9,
			$10, $11,
			$12, $13, $14, $15
		)
	`

	_, err := r.db.Exec(query,
		sales.SalesNo, sales.SalesDate, sales.SalesCategory, sales.OrderNo,
		sales.DepartmentCode, sales.StartDate, sales.CompanyCode, sales.SalesAmount, sales.ConsumptionTax,
		sales.CorrectionNo, sales.OriginalSlipNo,
		sales.CreatedAt, sales.CreatedBy, sales.UpdatedAt, sales.UpdatedBy)
	if err != nil {
		return fmt.Errorf("売上の登録に失敗しました: %w", err)
	}

	return nil
}

// FindByID は売上番号で売上を取得します
func (r *SalesRepository) FindByID(salesNo string) (*model.Sales, error) {
	query := `
		SELECT 売上番号, 売上日, 売上区分, 受注番号,
		       部門コード, 開始日, 取引先コード, 売上金額, 消費税,
		       訂正番号, 元伝票番号,
		       作成日時, 作成者名, 更新日時, 更新者名
		FROM 売上
		WHERE 売上番号 = $1
	`

	var sales model.Sales
	err := r.db.Get(&sales, query, salesNo)
	if err != nil {
		if err == sql.ErrNoRows {
			return nil, nil
		}
		return nil, fmt.Errorf("売上の取得に失敗しました: %w", err)
	}

	return &sales, nil
}

// FindByOrderNo は受注番号で売上を検索します
func (r *SalesRepository) FindByOrderNo(orderNo string) ([]*model.Sales, error) {
	query := `
		SELECT 売上番号, 売上日, 売上区分, 受注番号,
		       部門コード, 開始日, 取引先コード, 売上金額, 消費税,
		       訂正番号, 元伝票番号,
		       作成日時, 作成者名, 更新日時, 更新者名
		FROM 売上
		WHERE 受注番号 = $1
		ORDER BY 売上日 DESC
	`

	var salesList []*model.Sales
	err := r.db.Select(&salesList, query, orderNo)
	if err != nil {
		return nil, fmt.Errorf("受注番号別売上の取得に失敗しました: %w", err)
	}

	return salesList, nil
}

// FindAll はすべての売上を取得します
func (r *SalesRepository) FindAll() ([]*model.Sales, error) {
	query := `
		SELECT 売上番号, 売上日, 売上区分, 受注番号,
		       部門コード, 開始日, 取引先コード, 売上金額, 消費税,
		       訂正番号, 元伝票番号,
		       作成日時, 作成者名, 更新日時, 更新者名
		FROM 売上
		ORDER BY 売上日 DESC
	`

	var salesList []*model.Sales
	err := r.db.Select(&salesList, query)
	if err != nil {
		return nil, fmt.Errorf("売上一覧の取得に失敗しました: %w", err)
	}

	return salesList, nil
}

// Update は売上を更新します
func (r *SalesRepository) Update(sales *model.Sales) error {
	query := `
		UPDATE 売上
		SET 売上日 = $1,
		    売上区分 = $2,
		    売上金額 = $3,
		    消費税 = $4,
		    更新日時 = $5,
		    更新者名 = $6
		WHERE 売上番号 = $7
	`

	result, err := r.db.Exec(query,
		sales.SalesDate, sales.SalesCategory, sales.SalesAmount, sales.ConsumptionTax,
		sales.UpdatedAt, sales.UpdatedBy,
		sales.SalesNo)
	if err != nil {
		return fmt.Errorf("売上の更新に失敗しました: %w", err)
	}

	rowsAffected, err := result.RowsAffected()
	if err != nil {
		return fmt.Errorf("更新結果の取得に失敗しました: %w", err)
	}

	if rowsAffected == 0 {
		return fmt.Errorf("売上が見つかりません")
	}

	return nil
}

// Delete は売上を削除します
func (r *SalesRepository) Delete(salesNo string) error {
	query := `DELETE FROM 売上 WHERE 売上番号 = $1`

	result, err := r.db.Exec(query, salesNo)
	if err != nil {
		return fmt.Errorf("売上の削除に失敗しました: %w", err)
	}

	rowsAffected, err := result.RowsAffected()
	if err != nil {
		return fmt.Errorf("削除結果の取得に失敗しました: %w", err)
	}

	if rowsAffected == 0 {
		return fmt.Errorf("売上が見つかりません")
	}

	return nil
}
