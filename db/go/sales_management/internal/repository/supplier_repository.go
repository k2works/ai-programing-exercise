package repository

import (
	"database/sql"
	"fmt"

	"github.com/jmoiron/sqlx"
	"github.com/k2works/sales-management-db/internal/model"
)

// SupplierRepository は仕入先マスタのデータアクセスを提供します
type SupplierRepository struct {
	db *sqlx.DB
}

// NewSupplierRepository は新しいSupplierRepositoryを作成します
func NewSupplierRepository(db *sqlx.DB) *SupplierRepository {
	return &SupplierRepository{db: db}
}

// Create は仕入先を登録します
func (r *SupplierRepository) Create(supplier *model.Supplier) error {
	query := `
		INSERT INTO 仕入先マスタ (
			仕入先コード, 仕入先枝番, 仕入先区分,
			仕入先名, 仕入先名カナ, 担当社員コード,
			仕入先担当者名, 仕入先担当部署名, 仕入先郵便番号,
			仕入先都道府県, 仕入先住所１, 仕入先電話番号, 仕入先メールアドレス,
			仕入先締日, 仕入先支払月数, 仕入先支払日, 仕入先支払方法,
			作成日時, 作成者名, 更新日時, 更新者名
		) VALUES (
			$1, $2, $3,
			$4, $5, $6,
			$7, $8, $9,
			$10, $11, $12, $13,
			$14, $15, $16, $17,
			$18, $19, $20, $21
		)
	`

	_, err := r.db.Exec(query,
		supplier.SupplierCode, supplier.SupplierBranch, supplier.SupplierCategory,
		supplier.SupplierName, supplier.SupplierNameKana, supplier.EmployeeCode,
		supplier.SupplierContactName, supplier.SupplierDepartmentName, supplier.SupplierPostalCode,
		supplier.SupplierPrefecture, supplier.SupplierAddress1, supplier.SupplierPhone, supplier.SupplierEmail,
		supplier.ClosingDay, supplier.PaymentMonth, supplier.PaymentDay, supplier.PaymentMethod,
		supplier.CreatedAt, supplier.CreatedBy, supplier.UpdatedAt, supplier.UpdatedBy)
	if err != nil {
		return fmt.Errorf("仕入先の登録に失敗しました: %w", err)
	}

	return nil
}

// FindByID は仕入先コードと仕入先枝番で仕入先を取得します
func (r *SupplierRepository) FindByID(supplierCode string, supplierBranch int) (*model.Supplier, error) {
	query := `
		SELECT 仕入先コード, 仕入先枝番, 仕入先区分,
		       仕入先名, 仕入先名カナ, 担当社員コード,
		       仕入先担当者名, 仕入先担当部署名, 仕入先郵便番号,
		       仕入先都道府県, 仕入先住所１, 仕入先電話番号, 仕入先メールアドレス,
		       仕入先締日, 仕入先支払月数, 仕入先支払日, 仕入先支払方法,
		       作成日時, 作成者名, 更新日時, 更新者名
		FROM 仕入先マスタ
		WHERE 仕入先コード = $1 AND 仕入先枝番 = $2
	`

	var supplier model.Supplier
	err := r.db.Get(&supplier, query, supplierCode, supplierBranch)
	if err != nil {
		if err == sql.ErrNoRows {
			return nil, nil
		}
		return nil, fmt.Errorf("仕入先の取得に失敗しました: %w", err)
	}

	return &supplier, nil
}

// FindAll はすべての仕入先を取得します
func (r *SupplierRepository) FindAll() ([]*model.Supplier, error) {
	query := `
		SELECT 仕入先コード, 仕入先枝番, 仕入先区分,
		       仕入先名, 仕入先名カナ, 担当社員コード,
		       仕入先担当者名, 仕入先担当部署名, 仕入先郵便番号,
		       仕入先都道府県, 仕入先住所１, 仕入先電話番号, 仕入先メールアドレス,
		       仕入先締日, 仕入先支払月数, 仕入先支払日, 仕入先支払方法,
		       作成日時, 作成者名, 更新日時, 更新者名
		FROM 仕入先マスタ
		ORDER BY 仕入先コード, 仕入先枝番
	`

	var suppliers []*model.Supplier
	err := r.db.Select(&suppliers, query)
	if err != nil {
		return nil, fmt.Errorf("仕入先一覧の取得に失敗しました: %w", err)
	}

	return suppliers, nil
}

// Update は仕入先を更新します
func (r *SupplierRepository) Update(supplier *model.Supplier) error {
	query := `
		UPDATE 仕入先マスタ
		SET 仕入先区分 = $1,
		    仕入先名 = $2,
		    仕入先名カナ = $3,
		    担当社員コード = $4,
		    仕入先担当者名 = $5,
		    仕入先担当部署名 = $6,
		    仕入先郵便番号 = $7,
		    仕入先都道府県 = $8,
		    仕入先住所１ = $9,
		    仕入先電話番号 = $10,
		    仕入先メールアドレス = $11,
		    仕入先締日 = $12,
		    仕入先支払月数 = $13,
		    仕入先支払日 = $14,
		    仕入先支払方法 = $15,
		    更新日時 = $16,
		    更新者名 = $17
		WHERE 仕入先コード = $18 AND 仕入先枝番 = $19
	`

	result, err := r.db.Exec(query,
		supplier.SupplierCategory,
		supplier.SupplierName, supplier.SupplierNameKana, supplier.EmployeeCode,
		supplier.SupplierContactName, supplier.SupplierDepartmentName,
		supplier.SupplierPostalCode, supplier.SupplierPrefecture, supplier.SupplierAddress1,
		supplier.SupplierPhone, supplier.SupplierEmail,
		supplier.ClosingDay, supplier.PaymentMonth, supplier.PaymentDay, supplier.PaymentMethod,
		supplier.UpdatedAt, supplier.UpdatedBy,
		supplier.SupplierCode, supplier.SupplierBranch)
	if err != nil {
		return fmt.Errorf("仕入先の更新に失敗しました: %w", err)
	}

	rows, err := result.RowsAffected()
	if err != nil {
		return fmt.Errorf("更新結果の取得に失敗しました: %w", err)
	}

	if rows == 0 {
		return fmt.Errorf("仕入先が見つかりません: %s-%d", supplier.SupplierCode, supplier.SupplierBranch)
	}

	return nil
}

// Delete は仕入先を削除します
func (r *SupplierRepository) Delete(supplierCode string, supplierBranch int) error {
	query := `DELETE FROM 仕入先マスタ WHERE 仕入先コード = $1 AND 仕入先枝番 = $2`

	result, err := r.db.Exec(query, supplierCode, supplierBranch)
	if err != nil {
		return fmt.Errorf("仕入先の削除に失敗しました: %w", err)
	}

	rows, err := result.RowsAffected()
	if err != nil {
		return fmt.Errorf("削除結果の取得に失敗しました: %w", err)
	}

	if rows == 0 {
		return fmt.Errorf("仕入先が見つかりません: %s-%d", supplierCode, supplierBranch)
	}

	return nil
}
