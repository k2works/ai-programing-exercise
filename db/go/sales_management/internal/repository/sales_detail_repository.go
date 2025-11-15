package repository

import (
	"database/sql"
	"fmt"

	"github.com/jmoiron/sqlx"
	"github.com/k2works/sales-management-db/internal/model"
)

// SalesDetailRepository は売上明細データのデータアクセスを提供します
type SalesDetailRepository struct {
	db *sqlx.DB
}

// NewSalesDetailRepository は新しいSalesDetailRepositoryを作成します
func NewSalesDetailRepository(db *sqlx.DB) *SalesDetailRepository {
	return &SalesDetailRepository{db: db}
}

// Create は売上明細を登録します
func (r *SalesDetailRepository) Create(detail *model.SalesDetail) error {
	query := `
		INSERT INTO 売上明細 (
			売上番号, 明細番号, 商品コード, 商品名, 販売単価,
			出荷済数量, 数量, 値引額, 請求日, 請求番号,
			請求遅延区分, 自動仕訳日,
			作成日時, 作成者名, 更新日時, 更新者名
		) VALUES (
			$1, $2, $3, $4, $5,
			$6, $7, $8, $9, $10,
			$11, $12,
			$13, $14, $15, $16
		)
	`

	_, err := r.db.Exec(query,
		detail.SalesNo, detail.DetailNo, detail.ProductCode, detail.ProductName, detail.SellingPrice,
		detail.ShippedQuantity, detail.Quantity, detail.DiscountAmount, detail.BillingDate, detail.InvoiceNo,
		detail.BillingDelayFlag, detail.AutoJournalDate,
		detail.CreatedAt, detail.CreatedBy, detail.UpdatedAt, detail.UpdatedBy)
	if err != nil {
		return fmt.Errorf("売上明細の登録に失敗しました: %w", err)
	}

	return nil
}

// FindBySalesNo は売上番号で売上明細を取得します
func (r *SalesDetailRepository) FindBySalesNo(salesNo string) ([]*model.SalesDetail, error) {
	query := `
		SELECT 売上番号, 明細番号, 商品コード, 商品名, 販売単価,
		       出荷済数量, 数量, 値引額, 請求日, 請求番号,
		       請求遅延区分, 自動仕訳日,
		       作成日時, 作成者名, 更新日時, 更新者名
		FROM 売上明細
		WHERE 売上番号 = $1
		ORDER BY 明細番号
	`

	var details []*model.SalesDetail
	err := r.db.Select(&details, query, salesNo)
	if err != nil {
		return nil, fmt.Errorf("売上明細の取得に失敗しました: %w", err)
	}

	return details, nil
}

// FindByID は複合主キーで売上明細を取得します
func (r *SalesDetailRepository) FindByID(salesNo string, detailNo int) (*model.SalesDetail, error) {
	query := `
		SELECT 売上番号, 明細番号, 商品コード, 商品名, 販売単価,
		       出荷済数量, 数量, 値引額, 請求日, 請求番号,
		       請求遅延区分, 自動仕訳日,
		       作成日時, 作成者名, 更新日時, 更新者名
		FROM 売上明細
		WHERE 売上番号 = $1 AND 明細番号 = $2
	`

	var detail model.SalesDetail
	err := r.db.Get(&detail, query, salesNo, detailNo)
	if err != nil {
		if err == sql.ErrNoRows {
			return nil, nil
		}
		return nil, fmt.Errorf("売上明細の取得に失敗しました: %w", err)
	}

	return &detail, nil
}

// Update は売上明細を更新します
func (r *SalesDetailRepository) Update(detail *model.SalesDetail) error {
	query := `
		UPDATE 売上明細
		SET 数量 = $1,
		    値引額 = $2,
		    請求日 = $3,
		    請求番号 = $4,
		    更新日時 = $5,
		    更新者名 = $6
		WHERE 売上番号 = $7 AND 明細番号 = $8
	`

	result, err := r.db.Exec(query,
		detail.Quantity, detail.DiscountAmount, detail.BillingDate, detail.InvoiceNo,
		detail.UpdatedAt, detail.UpdatedBy,
		detail.SalesNo, detail.DetailNo)
	if err != nil {
		return fmt.Errorf("売上明細の更新に失敗しました: %w", err)
	}

	rowsAffected, err := result.RowsAffected()
	if err != nil {
		return fmt.Errorf("更新結果の取得に失敗しました: %w", err)
	}

	if rowsAffected == 0 {
		return fmt.Errorf("売上明細が見つかりません")
	}

	return nil
}

// Delete は売上明細を削除します
func (r *SalesDetailRepository) Delete(salesNo string, detailNo int) error {
	query := `DELETE FROM 売上明細 WHERE 売上番号 = $1 AND 明細番号 = $2`

	result, err := r.db.Exec(query, salesNo, detailNo)
	if err != nil {
		return fmt.Errorf("売上明細の削除に失敗しました: %w", err)
	}

	rowsAffected, err := result.RowsAffected()
	if err != nil {
		return fmt.Errorf("削除結果の取得に失敗しました: %w", err)
	}

	if rowsAffected == 0 {
		return fmt.Errorf("売上明細が見つかりません")
	}

	return nil
}
