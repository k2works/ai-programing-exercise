package repository

import (
	"database/sql"
	"fmt"

	"github.com/jmoiron/sqlx"
	"github.com/k2works/sales-management-db/internal/model"
)

// CompanyRepository は取引先マスタのデータアクセスを提供します
type CompanyRepository struct {
	db *sqlx.DB
}

// NewCompanyRepository は新しいCompanyRepositoryを作成します
func NewCompanyRepository(db *sqlx.DB) *CompanyRepository {
	return &CompanyRepository{db: db}
}

// Create は取引先を登録します
func (r *CompanyRepository) Create(company *model.Company) error {
	query := `
		INSERT INTO 取引先マスタ (
			取引先コード, 取引先名, 取引先名カナ, 仕入先区分,
			郵便番号, 都道府県, 住所１, 住所２,
			取引禁止フラグ, 雑区分, 取引先グループコード,
			与信限度額, 与信一時増加枠,
			作成日時, 作成者名, 更新日時, 更新者名
		) VALUES (
			$1, $2, $3, $4,
			$5, $6, $7, $8,
			$9, $10, $11,
			$12, $13,
			$14, $15, $16, $17
		)
	`

	_, err := r.db.Exec(query,
		company.CompanyCode, company.CompanyName, company.CompanyNameKana, company.SupplierCategory,
		company.PostalCode, company.Prefecture, company.Address1, company.Address2,
		company.TransactionProhibit, company.MiscCategory, company.CompanyGroupCode,
		company.CreditLimit, company.TemporaryCreditBoost,
		company.CreatedAt, company.CreatedBy, company.UpdatedAt, company.UpdatedBy)
	if err != nil {
		return fmt.Errorf("取引先の登録に失敗しました: %w", err)
	}

	return nil
}

// FindByID は取引先コードで取引先を取得します
func (r *CompanyRepository) FindByID(companyCode string) (*model.Company, error) {
	query := `
		SELECT 取引先コード, 取引先名, 取引先名カナ, 仕入先区分,
		       郵便番号, 都道府県, 住所１, 住所２,
		       取引禁止フラグ, 雑区分, 取引先グループコード,
		       与信限度額, 与信一時増加枠,
		       作成日時, 作成者名, 更新日時, 更新者名
		FROM 取引先マスタ
		WHERE 取引先コード = $1
	`

	var company model.Company
	err := r.db.Get(&company, query, companyCode)
	if err != nil {
		if err == sql.ErrNoRows {
			return nil, nil
		}
		return nil, fmt.Errorf("取引先の取得に失敗しました: %w", err)
	}

	return &company, nil
}

// FindAll はすべての取引先を取得します
func (r *CompanyRepository) FindAll() ([]*model.Company, error) {
	query := `
		SELECT 取引先コード, 取引先名, 取引先名カナ, 仕入先区分,
		       郵便番号, 都道府県, 住所１, 住所２,
		       取引禁止フラグ, 雑区分, 取引先グループコード,
		       与信限度額, 与信一時増加枠,
		       作成日時, 作成者名, 更新日時, 更新者名
		FROM 取引先マスタ
		ORDER BY 取引先コード
	`

	var companies []*model.Company
	err := r.db.Select(&companies, query)
	if err != nil {
		return nil, fmt.Errorf("取引先一覧の取得に失敗しました: %w", err)
	}

	return companies, nil
}

// Update は取引先を更新します
func (r *CompanyRepository) Update(company *model.Company) error {
	query := `
		UPDATE 取引先マスタ
		SET 取引先名 = $1,
		    取引先名カナ = $2,
		    仕入先区分 = $3,
		    郵便番号 = $4,
		    都道府県 = $5,
		    住所１ = $6,
		    住所２ = $7,
		    取引禁止フラグ = $8,
		    雑区分 = $9,
		    取引先グループコード = $10,
		    与信限度額 = $11,
		    与信一時増加枠 = $12,
		    更新日時 = $13,
		    更新者名 = $14
		WHERE 取引先コード = $15
	`

	result, err := r.db.Exec(query,
		company.CompanyName, company.CompanyNameKana, company.SupplierCategory,
		company.PostalCode, company.Prefecture, company.Address1, company.Address2,
		company.TransactionProhibit, company.MiscCategory, company.CompanyGroupCode,
		company.CreditLimit, company.TemporaryCreditBoost,
		company.UpdatedAt, company.UpdatedBy,
		company.CompanyCode)
	if err != nil {
		return fmt.Errorf("取引先の更新に失敗しました: %w", err)
	}

	rows, err := result.RowsAffected()
	if err != nil {
		return fmt.Errorf("更新結果の取得に失敗しました: %w", err)
	}

	if rows == 0 {
		return fmt.Errorf("取引先が見つかりません: %s", company.CompanyCode)
	}

	return nil
}

// Delete は取引先を削除します
func (r *CompanyRepository) Delete(companyCode string) error {
	query := `DELETE FROM 取引先マスタ WHERE 取引先コード = $1`

	result, err := r.db.Exec(query, companyCode)
	if err != nil {
		return fmt.Errorf("取引先の削除に失敗しました: %w", err)
	}

	rows, err := result.RowsAffected()
	if err != nil {
		return fmt.Errorf("削除結果の取得に失敗しました: %w", err)
	}

	if rows == 0 {
		return fmt.Errorf("取引先が見つかりません: %s", companyCode)
	}

	return nil
}
