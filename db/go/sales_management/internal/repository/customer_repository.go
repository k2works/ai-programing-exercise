package repository

import (
	"database/sql"
	"fmt"

	"github.com/jmoiron/sqlx"
	"github.com/k2works/sales-management-db/internal/model"
)

// CustomerRepository は顧客マスタのデータアクセスを提供します
type CustomerRepository struct {
	db *sqlx.DB
}

// NewCustomerRepository は新しいCustomerRepositoryを作成します
func NewCustomerRepository(db *sqlx.DB) *CustomerRepository {
	return &CustomerRepository{db: db}
}

// Create は顧客を登録します
func (r *CustomerRepository) Create(customer *model.Customer) error {
	query := `
		INSERT INTO 顧客マスタ (
			顧客コード, 顧客枝番, 顧客区分,
			請求先コード, 請求先枝番, 回収先コード, 回収先枝番,
			顧客名, 顧客名カナ, 担当社員コード,
			顧客担当者名, 顧客担当部署名, 顧客郵便番号,
			顧客都道府県, 顧客住所１, 顧客電話番号, 顧客メールアドレス,
			顧客締日１, 顧客支払月数１, 顧客支払日１, 顧客支払方法１, 顧客締日２,
			作成日時, 作成者名, 更新日時, 更新者名
		) VALUES (
			$1, $2, $3,
			$4, $5, $6, $7,
			$8, $9, $10,
			$11, $12, $13,
			$14, $15, $16, $17,
			$18, $19, $20, $21, $22,
			$23, $24, $25, $26
		)
	`

	_, err := r.db.Exec(query,
		customer.CustomerCode, customer.CustomerBranch, customer.CustomerCategory,
		customer.BillingCode, customer.BillingBranch, customer.CollectionCode, customer.CollectionBranch,
		customer.CustomerName, customer.CustomerNameKana, customer.SalesEmployeeCode,
		customer.CustomerContactName, customer.CustomerDepartmentName, customer.CustomerPostalCode,
		customer.CustomerPrefecture, customer.CustomerAddress1, customer.CustomerPhone, customer.CustomerEmail,
		customer.ClosingDay1, customer.PaymentMonth1, customer.PaymentDay1, customer.PaymentMethod1, customer.ClosingDay2,
		customer.CreatedAt, customer.CreatedBy, customer.UpdatedAt, customer.UpdatedBy)
	if err != nil {
		return fmt.Errorf("顧客の登録に失敗しました: %w", err)
	}

	return nil
}

// FindByID は顧客コードと顧客枝番で顧客を取得します
func (r *CustomerRepository) FindByID(customerCode string, customerBranch int) (*model.Customer, error) {
	query := `
		SELECT 顧客コード, 顧客枝番, 顧客区分,
		       請求先コード, 請求先枝番, 回収先コード, 回収先枝番,
		       顧客名, 顧客名カナ, 担当社員コード,
		       顧客担当者名, 顧客担当部署名, 顧客郵便番号,
		       顧客都道府県, 顧客住所１, 顧客電話番号, 顧客メールアドレス,
		       顧客締日１, 顧客支払月数１, 顧客支払日１, 顧客支払方法１, 顧客締日２,
		       作成日時, 作成者名, 更新日時, 更新者名
		FROM 顧客マスタ
		WHERE 顧客コード = $1 AND 顧客枝番 = $2
	`

	var customer model.Customer
	err := r.db.Get(&customer, query, customerCode, customerBranch)
	if err != nil {
		if err == sql.ErrNoRows {
			return nil, nil
		}
		return nil, fmt.Errorf("顧客の取得に失敗しました: %w", err)
	}

	return &customer, nil
}

// FindAll はすべての顧客を取得します
func (r *CustomerRepository) FindAll() ([]*model.Customer, error) {
	query := `
		SELECT 顧客コード, 顧客枝番, 顧客区分,
		       請求先コード, 請求先枝番, 回収先コード, 回収先枝番,
		       顧客名, 顧客名カナ, 担当社員コード,
		       顧客担当者名, 顧客担当部署名, 顧客郵便番号,
		       顧客都道府県, 顧客住所１, 顧客電話番号, 顧客メールアドレス,
		       顧客締日１, 顧客支払月数１, 顧客支払日１, 顧客支払方法１, 顧客締日２,
		       作成日時, 作成者名, 更新日時, 更新者名
		FROM 顧客マスタ
		ORDER BY 顧客コード, 顧客枝番
	`

	var customers []*model.Customer
	err := r.db.Select(&customers, query)
	if err != nil {
		return nil, fmt.Errorf("顧客一覧の取得に失敗しました: %w", err)
	}

	return customers, nil
}

// Update は顧客を更新します
func (r *CustomerRepository) Update(customer *model.Customer) error {
	query := `
		UPDATE 顧客マスタ
		SET 顧客区分 = $1,
		    請求先コード = $2,
		    請求先枝番 = $3,
		    回収先コード = $4,
		    回収先枝番 = $5,
		    顧客名 = $6,
		    顧客名カナ = $7,
		    担当社員コード = $8,
		    顧客担当者名 = $9,
		    顧客担当部署名 = $10,
		    顧客郵便番号 = $11,
		    顧客都道府県 = $12,
		    顧客住所１ = $13,
		    顧客電話番号 = $14,
		    顧客メールアドレス = $15,
		    顧客締日１ = $16,
		    顧客支払月数１ = $17,
		    顧客支払日１ = $18,
		    顧客支払方法１ = $19,
		    顧客締日２ = $20,
		    更新日時 = $21,
		    更新者名 = $22
		WHERE 顧客コード = $23 AND 顧客枝番 = $24
	`

	result, err := r.db.Exec(query,
		customer.CustomerCategory,
		customer.BillingCode, customer.BillingBranch,
		customer.CollectionCode, customer.CollectionBranch,
		customer.CustomerName, customer.CustomerNameKana, customer.SalesEmployeeCode,
		customer.CustomerContactName, customer.CustomerDepartmentName,
		customer.CustomerPostalCode, customer.CustomerPrefecture, customer.CustomerAddress1,
		customer.CustomerPhone, customer.CustomerEmail,
		customer.ClosingDay1, customer.PaymentMonth1, customer.PaymentDay1, customer.PaymentMethod1, customer.ClosingDay2,
		customer.UpdatedAt, customer.UpdatedBy,
		customer.CustomerCode, customer.CustomerBranch)
	if err != nil {
		return fmt.Errorf("顧客の更新に失敗しました: %w", err)
	}

	rows, err := result.RowsAffected()
	if err != nil {
		return fmt.Errorf("更新結果の取得に失敗しました: %w", err)
	}

	if rows == 0 {
		return fmt.Errorf("顧客が見つかりません: %s-%d", customer.CustomerCode, customer.CustomerBranch)
	}

	return nil
}

// Delete は顧客を削除します
func (r *CustomerRepository) Delete(customerCode string, customerBranch int) error {
	query := `DELETE FROM 顧客マスタ WHERE 顧客コード = $1 AND 顧客枝番 = $2`

	result, err := r.db.Exec(query, customerCode, customerBranch)
	if err != nil {
		return fmt.Errorf("顧客の削除に失敗しました: %w", err)
	}

	rows, err := result.RowsAffected()
	if err != nil {
		return fmt.Errorf("削除結果の取得に失敗しました: %w", err)
	}

	if rows == 0 {
		return fmt.Errorf("顧客が見つかりません: %s-%d", customerCode, customerBranch)
	}

	return nil
}
