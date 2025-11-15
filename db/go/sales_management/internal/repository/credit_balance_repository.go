package repository

import (
	"database/sql"

	"github.com/jmoiron/sqlx"
	"github.com/k2works/sales-management-db/internal/model"
)

// CreditBalanceRepository 与信残高リポジトリ
type CreditBalanceRepository struct {
	db *sqlx.DB
}

// NewCreditBalanceRepository リポジトリの生成
func NewCreditBalanceRepository(db *sqlx.DB) *CreditBalanceRepository {
	return &CreditBalanceRepository{db: db}
}

// Create 与信残高を登録する
func (r *CreditBalanceRepository) Create(balance *model.CreditBalance) error {
	query := `
		INSERT INTO "与信残高" (
			"取引先コード", "受注残高", "売掛残高", "買掛残高",
			"作成日時", "作成者名", "更新日時", "更新者名"
		) VALUES (
			$1, $2, $3, $4, $5, $6, $7, $8
		)
	`
	_, err := r.db.Exec(query,
		balance.CompanyCode,
		balance.OrderBalance,
		balance.AccountsReceivable,
		balance.AccountsPayable,
		balance.CreatedAt,
		balance.CreatedBy,
		balance.UpdatedAt,
		balance.UpdatedBy,
	)
	return err
}

// FindByCompanyCode 取引先コードで与信残高を取得する
func (r *CreditBalanceRepository) FindByCompanyCode(companyCode string) (*model.CreditBalance, error) {
	query := `
		SELECT "取引先コード", "受注残高", "売掛残高", "買掛残高",
		       "作成日時", "作成者名", "更新日時", "更新者名"
		FROM "与信残高"
		WHERE "取引先コード" = $1
	`
	var balance model.CreditBalance
	err := r.db.Get(&balance, query, companyCode)
	if err == sql.ErrNoRows {
		return nil, nil
	}
	if err != nil {
		return nil, err
	}
	return &balance, nil
}

// FindAll 全ての与信残高を取得する
func (r *CreditBalanceRepository) FindAll() ([]*model.CreditBalance, error) {
	query := `
		SELECT "取引先コード", "受注残高", "売掛残高", "買掛残高",
		       "作成日時", "作成者名", "更新日時", "更新者名"
		FROM "与信残高"
		ORDER BY "取引先コード"
	`
	var balances []*model.CreditBalance
	err := r.db.Select(&balances, query)
	return balances, err
}

// Update 与信残高を更新する
func (r *CreditBalanceRepository) Update(balance *model.CreditBalance) error {
	query := `
		UPDATE "与信残高" SET
			"受注残高" = $1,
			"売掛残高" = $2,
			"買掛残高" = $3,
			"更新日時" = $4,
			"更新者名" = $5
		WHERE "取引先コード" = $6
	`
	_, err := r.db.Exec(query,
		balance.OrderBalance,
		balance.AccountsReceivable,
		balance.AccountsPayable,
		balance.UpdatedAt,
		balance.UpdatedBy,
		balance.CompanyCode,
	)
	return err
}

// Delete 与信残高を削除する
func (r *CreditBalanceRepository) Delete(companyCode string) error {
	query := `DELETE FROM "与信残高" WHERE "取引先コード" = $1`
	_, err := r.db.Exec(query, companyCode)
	return err
}
