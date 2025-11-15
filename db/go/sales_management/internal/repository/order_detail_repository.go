package repository

import (
	"database/sql"
	"fmt"

	"github.com/jmoiron/sqlx"
	"github.com/k2works/sales-management-db/internal/model"
)

// OrderDetailRepository は受注明細のデータアクセスを提供します
type OrderDetailRepository struct {
	db *sqlx.DB
}

// NewOrderDetailRepository は新しいOrderDetailRepositoryを作成します
func NewOrderDetailRepository(db *sqlx.DB) *OrderDetailRepository {
	return &OrderDetailRepository{db: db}
}

// Create は受注明細を登録します
func (r *OrderDetailRepository) Create(detail *model.OrderDetail) error {
	query := `
		INSERT INTO 受注明細 (
			受注番号, 明細番号, 商品コード, 商品名, 販売単価, 数量,
			消費税率, 引当数量, 出荷指示数量, 出荷済数量, 完了フラグ,
			値引額, 納品日, 作成日時, 作成者名, 更新日時, 更新者名
		) VALUES (
			$1, $2, $3, $4, $5, $6,
			$7, $8, $9, $10, $11,
			$12, $13, $14, $15, $16, $17
		)
	`

	_, err := r.db.Exec(query,
		detail.OrderNo, detail.DetailNo, detail.ProductCode, detail.ProductName,
		detail.SalesUnitPrice, detail.Quantity, detail.ConsumptionTaxRate,
		detail.AllocatedQuantity, detail.ShippingOrderQuantity, detail.ShippedQuantity,
		detail.CompletionFlag, detail.DiscountAmount, detail.DeliveryDate,
		detail.CreatedAt, detail.CreatedBy, detail.UpdatedAt, detail.UpdatedBy)
	if err != nil {
		return fmt.Errorf("受注明細の登録に失敗しました: %w", err)
	}

	return nil
}

// FindByOrderNo は受注番号で受注明細を取得します
func (r *OrderDetailRepository) FindByOrderNo(orderNo string) ([]*model.OrderDetail, error) {
	query := `
		SELECT 受注番号, 明細番号, 商品コード, 商品名, 販売単価, 数量,
		       消費税率, 引当数量, 出荷指示数量, 出荷済数量, 完了フラグ,
		       値引額, 納品日, 作成日時, 作成者名, 更新日時, 更新者名
		FROM 受注明細
		WHERE 受注番号 = $1
		ORDER BY 明細番号
	`

	var details []*model.OrderDetail
	err := r.db.Select(&details, query, orderNo)
	if err != nil {
		return nil, fmt.Errorf("受注明細の取得に失敗しました: %w", err)
	}

	return details, nil
}

// FindByID は受注番号と明細番号で受注明細を取得します
func (r *OrderDetailRepository) FindByID(orderNo string, detailNo int) (*model.OrderDetail, error) {
	query := `
		SELECT 受注番号, 明細番号, 商品コード, 商品名, 販売単価, 数量,
		       消費税率, 引当数量, 出荷指示数量, 出荷済数量, 完了フラグ,
		       値引額, 納品日, 作成日時, 作成者名, 更新日時, 更新者名
		FROM 受注明細
		WHERE 受注番号 = $1 AND 明細番号 = $2
	`

	var detail model.OrderDetail
	err := r.db.Get(&detail, query, orderNo, detailNo)
	if err != nil {
		if err == sql.ErrNoRows {
			return nil, nil
		}
		return nil, fmt.Errorf("受注明細の取得に失敗しました: %w", err)
	}

	return &detail, nil
}

// Update は受注明細を更新します
func (r *OrderDetailRepository) Update(detail *model.OrderDetail) error {
	query := `
		UPDATE 受注明細
		SET 数量 = $1,
		    引当数量 = $2,
		    出荷指示数量 = $3,
		    出荷済数量 = $4,
		    完了フラグ = $5,
		    値引額 = $6,
		    納品日 = $7,
		    更新日時 = $8,
		    更新者名 = $9
		WHERE 受注番号 = $10 AND 明細番号 = $11
	`

	result, err := r.db.Exec(query,
		detail.Quantity, detail.AllocatedQuantity, detail.ShippingOrderQuantity,
		detail.ShippedQuantity, detail.CompletionFlag, detail.DiscountAmount,
		detail.DeliveryDate, detail.UpdatedAt, detail.UpdatedBy,
		detail.OrderNo, detail.DetailNo)
	if err != nil {
		return fmt.Errorf("受注明細の更新に失敗しました: %w", err)
	}

	rowsAffected, err := result.RowsAffected()
	if err != nil {
		return fmt.Errorf("更新結果の取得に失敗しました: %w", err)
	}

	if rowsAffected == 0 {
		return fmt.Errorf("受注明細が見つかりません: %s-%d", detail.OrderNo, detail.DetailNo)
	}

	return nil
}

// Delete は受注明細を削除します
func (r *OrderDetailRepository) Delete(orderNo string, detailNo int) error {
	query := `DELETE FROM 受注明細 WHERE 受注番号 = $1 AND 明細番号 = $2`

	result, err := r.db.Exec(query, orderNo, detailNo)
	if err != nil {
		return fmt.Errorf("受注明細の削除に失敗しました: %w", err)
	}

	rowsAffected, err := result.RowsAffected()
	if err != nil {
		return fmt.Errorf("削除結果の取得に失敗しました: %w", err)
	}

	if rowsAffected == 0 {
		return fmt.Errorf("受注明細が見つかりません: %s-%d", orderNo, detailNo)
	}

	return nil
}
