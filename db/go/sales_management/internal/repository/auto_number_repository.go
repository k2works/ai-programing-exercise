package repository

import (
	"database/sql"
	"fmt"
	"time"

	"github.com/jmoiron/sqlx"
	"github.com/k2works/sales-management-db/internal/model"
)

// AutoNumberRepository 自動採番リポジトリ
type AutoNumberRepository struct {
	db *sqlx.DB
}

// NewAutoNumberRepository リポジトリの生成
func NewAutoNumberRepository(db *sqlx.DB) *AutoNumberRepository {
	return &AutoNumberRepository{db: db}
}

// Create 自動採番マスタを登録する
func (r *AutoNumberRepository) Create(autoNumber *model.AutoNumber) error {
	query := `
		INSERT INTO "自動採番" ("伝票種別", "年月", "最終伝票番号")
		VALUES ($1, $2, $3)
	`
	_, err := r.db.Exec(query,
		autoNumber.SlipType,
		autoNumber.YearMonth,
		autoNumber.LastSlipNumber,
	)
	return err
}

// FindByID 伝票種別と年月で自動採番マスタを取得する
func (r *AutoNumberRepository) FindByID(slipType string, yearMonth time.Time) (*model.AutoNumber, error) {
	query := `
		SELECT "伝票種別", "年月", "最終伝票番号"
		FROM "自動採番"
		WHERE "伝票種別" = $1 AND "年月" = $2
	`
	var autoNumber model.AutoNumber
	err := r.db.Get(&autoNumber, query, slipType, yearMonth)
	if err == sql.ErrNoRows {
		return nil, nil
	}
	if err != nil {
		return nil, err
	}
	return &autoNumber, nil
}

// GetNextNumber 次の伝票番号を取得する（排他制御付き）
//
// トランザクション内でFOR UPDATEによる排他ロックを取得して採番を行います。
func (r *AutoNumberRepository) GetNextNumber(tx *sqlx.Tx, slipType string, yearMonth time.Time) (int, error) {
	// FOR UPDATEで排他ロック
	query := `
		SELECT "伝票種別", "年月", "最終伝票番号"
		FROM "自動採番"
		WHERE "伝票種別" = $1 AND "年月" = $2
		FOR UPDATE
	`
	var autoNumber model.AutoNumber
	err := tx.Get(&autoNumber, query, slipType, yearMonth)
	if err != nil {
		return 0, fmt.Errorf("failed to get auto number: %w", err)
	}

	// 番号をインクリメント
	newNo := autoNumber.LastSlipNumber + 1

	// 更新
	updateQuery := `
		UPDATE "自動採番"
		SET "最終伝票番号" = $1
		WHERE "伝票種別" = $2 AND "年月" = $3
	`
	_, err = tx.Exec(updateQuery, newNo, slipType, yearMonth)
	if err != nil {
		return 0, fmt.Errorf("failed to update auto number: %w", err)
	}

	return newNo, nil
}

// Delete 自動採番マスタを削除する
func (r *AutoNumberRepository) Delete(slipType string, yearMonth time.Time) error {
	query := `
		DELETE FROM "自動採番"
		WHERE "伝票種別" = $1 AND "年月" = $2
	`
	_, err := r.db.Exec(query, slipType, yearMonth)
	return err
}
