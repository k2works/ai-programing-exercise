package repository

import (
	"database/sql"
	"fmt"

	"github.com/jmoiron/sqlx"
	"github.com/k2works/sales-management-db/internal/model"
)

// SalesOrderRepository は受注データのデータアクセスを提供します
type SalesOrderRepository struct {
	db *sqlx.DB
}

// NewSalesOrderRepository は新しいSalesOrderRepositoryを作成します
func NewSalesOrderRepository(db *sqlx.DB) *SalesOrderRepository {
	return &SalesOrderRepository{db: db}
}

// Create は受注を登録します
func (r *SalesOrderRepository) Create(order *model.SalesOrder) error {
	query := `
		INSERT INTO 受注 (
			受注番号, 受注日, 部門コード, 開始日, 顧客コード, 顧客枝番,
			社員コード, 納期, 顧客注文番号, 倉庫コード, 受注金額, 消費税,
			伝票コメント, 作成日時, 作成者名, 更新日時, 更新者名
		) VALUES (
			$1, $2, $3, $4, $5, $6,
			$7, $8, $9, $10, $11, $12,
			$13, $14, $15, $16, $17
		)
	`

	_, err := r.db.Exec(query,
		order.OrderNo, order.OrderDate, order.DepartmentCode, order.StartDate,
		order.CustomerCode, order.CustomerBranch, order.EmployeeCode,
		order.DeliveryDate, order.CustomerOrderNo, order.WarehouseCode,
		order.OrderAmount, order.ConsumptionTax, order.DocumentComment,
		order.CreatedAt, order.CreatedBy, order.UpdatedAt, order.UpdatedBy)
	if err != nil {
		return fmt.Errorf("受注の登録に失敗しました: %w", err)
	}

	return nil
}

// FindByID は受注番号で受注を取得します
func (r *SalesOrderRepository) FindByID(orderNo string) (*model.SalesOrder, error) {
	query := `
		SELECT 受注番号, 受注日, 部門コード, 開始日, 顧客コード, 顧客枝番,
		       社員コード, 納期, 顧客注文番号, 倉庫コード, 受注金額, 消費税,
		       伝票コメント, 作成日時, 作成者名, 更新日時, 更新者名
		FROM 受注
		WHERE 受注番号 = $1
	`

	var order model.SalesOrder
	err := r.db.Get(&order, query, orderNo)
	if err != nil {
		if err == sql.ErrNoRows {
			return nil, nil
		}
		return nil, fmt.Errorf("受注の取得に失敗しました: %w", err)
	}

	return &order, nil
}

// FindAll はすべての受注を取得します
func (r *SalesOrderRepository) FindAll() ([]*model.SalesOrder, error) {
	query := `
		SELECT 受注番号, 受注日, 部門コード, 開始日, 顧客コード, 顧客枝番,
		       社員コード, 納期, 顧客注文番号, 倉庫コード, 受注金額, 消費税,
		       伝票コメント, 作成日時, 作成者名, 更新日時, 更新者名
		FROM 受注
		ORDER BY 受注日 DESC, 受注番号
	`

	var orders []*model.SalesOrder
	err := r.db.Select(&orders, query)
	if err != nil {
		return nil, fmt.Errorf("受注一覧の取得に失敗しました: %w", err)
	}

	return orders, nil
}

// FindByCustomer は顧客コードで受注を検索します
func (r *SalesOrderRepository) FindByCustomer(custCode string, custBranch int) ([]*model.SalesOrder, error) {
	query := `
		SELECT 受注番号, 受注日, 部門コード, 開始日, 顧客コード, 顧客枝番,
		       社員コード, 納期, 顧客注文番号, 倉庫コード, 受注金額, 消費税,
		       伝票コメント, 作成日時, 作成者名, 更新日時, 更新者名
		FROM 受注
		WHERE 顧客コード = $1 AND 顧客枝番 = $2
		ORDER BY 受注日 DESC
	`

	var orders []*model.SalesOrder
	err := r.db.Select(&orders, query, custCode, custBranch)
	if err != nil {
		return nil, fmt.Errorf("顧客別受注の取得に失敗しました: %w", err)
	}

	return orders, nil
}

// Update は受注を更新します
func (r *SalesOrderRepository) Update(order *model.SalesOrder) error {
	query := `
		UPDATE 受注
		SET 受注日 = $1,
		    納期 = $2,
		    受注金額 = $3,
		    消費税 = $4,
		    伝票コメント = $5,
		    更新日時 = $6,
		    更新者名 = $7
		WHERE 受注番号 = $8
	`

	result, err := r.db.Exec(query,
		order.OrderDate, order.DeliveryDate,
		order.OrderAmount, order.ConsumptionTax, order.DocumentComment,
		order.UpdatedAt, order.UpdatedBy, order.OrderNo)
	if err != nil {
		return fmt.Errorf("受注の更新に失敗しました: %w", err)
	}

	rowsAffected, err := result.RowsAffected()
	if err != nil {
		return fmt.Errorf("更新結果の取得に失敗しました: %w", err)
	}

	if rowsAffected == 0 {
		return fmt.Errorf("受注が見つかりません: %s", order.OrderNo)
	}

	return nil
}

// Delete は受注を削除します
func (r *SalesOrderRepository) Delete(orderNo string) error {
	query := `DELETE FROM 受注 WHERE 受注番号 = $1`

	result, err := r.db.Exec(query, orderNo)
	if err != nil {
		return fmt.Errorf("受注の削除に失敗しました: %w", err)
	}

	rowsAffected, err := result.RowsAffected()
	if err != nil {
		return fmt.Errorf("削除結果の取得に失敗しました: %w", err)
	}

	if rowsAffected == 0 {
		return fmt.Errorf("受注が見つかりません: %s", orderNo)
	}

	return nil
}
