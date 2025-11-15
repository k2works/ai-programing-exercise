package repository

import (
	"context"
	"database/sql"
	"fmt"

	"github.com/k2works/sales-management-db/internal/domain"
	"github.com/k2works/sales-management-db/pkg/database"
)

// APIProductRepositoryImpl は APIProductRepository の実装です
type APIProductRepositoryImpl struct{}

// NewAPIProductRepository は新しい APIProductRepositoryImpl を作成します
func NewAPIProductRepository() APIProductRepository {
	return &APIProductRepositoryImpl{}
}

// Create は商品を登録します
func (r *APIProductRepositoryImpl) Create(ctx context.Context, tx *database.Tx, product *domain.Product) error {
	query := `
		INSERT INTO "商品マスタ" (
			"商品コード", "商品正式名", "商品略称", "商品名カナ",
			"販売単価", "仕入単価", "仕入先コード", "商品分類コード",
			"作成日時", "作成者名", "更新日時", "更新者名"
		) VALUES (
			$1, $2, $3, $4,
			$5, $6, $7, $8,
			$9, $10, $11, $12
		)
	`

	var supCode, categoryCode, kana, creator, updater sql.NullString

	if product.SupCode != nil {
		supCode = sql.NullString{String: *product.SupCode, Valid: true}
	}
	if product.CategoryCode != nil {
		categoryCode = sql.NullString{String: *product.CategoryCode, Valid: true}
	}
	if product.Kana != nil {
		kana = sql.NullString{String: *product.Kana, Valid: true}
	}
	if product.Creator != nil {
		creator = sql.NullString{String: *product.Creator, Valid: true}
	}
	if product.Updater != nil {
		updater = sql.NullString{String: *product.Updater, Valid: true}
	}

	_, err := tx.ExecContext(ctx, query,
		product.ProdCode, product.FullName, product.Name, kana,
		product.UnitPrice, product.PoPrice, supCode, categoryCode,
		product.CreateDate, creator, product.UpdateDate, updater)
	if err != nil {
		return fmt.Errorf("商品の登録に失敗しました: %w", err)
	}

	return nil
}

// FindByID は商品コードで商品を取得します
func (r *APIProductRepositoryImpl) FindByID(ctx context.Context, db database.Queryer, prodCode string) (*domain.Product, error) {
	query := `
		SELECT "商品コード", "商品正式名", "商品略称", "商品名カナ",
		       "販売単価", "仕入単価", "仕入先コード", "商品分類コード",
		       "作成日時", "作成者名", "更新日時", "更新者名"
		FROM "商品マスタ"
		WHERE "商品コード" = $1
	`

	var product domain.Product
	var kana, supCode, categoryCode, creator, updater sql.NullString

	err := db.QueryRowxContext(ctx, query, prodCode).Scan(
		&product.ProdCode, &product.FullName, &product.Name, &kana,
		&product.UnitPrice, &product.PoPrice, &supCode, &categoryCode,
		&product.CreateDate, &creator, &product.UpdateDate, &updater,
	)
	if err != nil {
		if err == sql.ErrNoRows {
			return nil, nil
		}
		return nil, fmt.Errorf("商品の取得に失敗しました: %w", err)
	}

	if kana.Valid {
		product.Kana = &kana.String
	}
	if supCode.Valid {
		product.SupCode = &supCode.String
	}
	if categoryCode.Valid {
		product.CategoryCode = &categoryCode.String
	}
	if creator.Valid {
		product.Creator = &creator.String
	}
	if updater.Valid {
		product.Updater = &updater.String
	}

	return &product, nil
}

// FindAll はすべての商品を取得します
func (r *APIProductRepositoryImpl) FindAll(ctx context.Context, db database.Queryer) ([]*domain.Product, error) {
	query := `
		SELECT "商品コード", "商品正式名", "商品略称", "商品名カナ",
		       "販売単価", "仕入単価", "仕入先コード", "商品分類コード",
		       "作成日時", "作成者名", "更新日時", "更新者名"
		FROM "商品マスタ"
		ORDER BY "商品コード"
	`

	rows, err := db.QueryxContext(ctx, query)
	if err != nil {
		return nil, fmt.Errorf("商品一覧の取得に失敗しました: %w", err)
	}
	defer rows.Close()

	var products []*domain.Product
	for rows.Next() {
		var product domain.Product
		var kana, supCode, categoryCode, creator, updater sql.NullString

		err := rows.Scan(
			&product.ProdCode, &product.FullName, &product.Name, &kana,
			&product.UnitPrice, &product.PoPrice, &supCode, &categoryCode,
			&product.CreateDate, &creator, &product.UpdateDate, &updater,
		)
		if err != nil {
			return nil, fmt.Errorf("商品データの読み取りに失敗しました: %w", err)
		}

		if kana.Valid {
			product.Kana = &kana.String
		}
		if supCode.Valid {
			product.SupCode = &supCode.String
		}
		if categoryCode.Valid {
			product.CategoryCode = &categoryCode.String
		}
		if creator.Valid {
			product.Creator = &creator.String
		}
		if updater.Valid {
			product.Updater = &updater.String
		}

		products = append(products, &product)
	}

	if err = rows.Err(); err != nil {
		return nil, fmt.Errorf("商品一覧の処理に失敗しました: %w", err)
	}

	return products, nil
}

// Update は商品を更新します
func (r *APIProductRepositoryImpl) Update(ctx context.Context, tx *database.Tx, product *domain.Product) error {
	query := `
		UPDATE "商品マスタ"
		SET "商品正式名" = $1,
		    "商品略称" = $2,
		    "商品名カナ" = $3,
		    "販売単価" = $4,
		    "仕入単価" = $5,
		    "仕入先コード" = $6,
		    "商品分類コード" = $7,
		    "更新日時" = $8,
		    "更新者名" = $9
		WHERE "商品コード" = $10
	`

	var kana, supCode, categoryCode, updater sql.NullString

	if product.Kana != nil {
		kana = sql.NullString{String: *product.Kana, Valid: true}
	}
	if product.SupCode != nil {
		supCode = sql.NullString{String: *product.SupCode, Valid: true}
	}
	if product.CategoryCode != nil {
		categoryCode = sql.NullString{String: *product.CategoryCode, Valid: true}
	}
	if product.Updater != nil {
		updater = sql.NullString{String: *product.Updater, Valid: true}
	}

	result, err := tx.ExecContext(ctx, query,
		product.FullName, product.Name, kana,
		product.UnitPrice, product.PoPrice, supCode, categoryCode,
		product.UpdateDate, updater,
		product.ProdCode)
	if err != nil {
		return fmt.Errorf("商品の更新に失敗しました: %w", err)
	}

	rows, err := result.RowsAffected()
	if err != nil {
		return fmt.Errorf("更新結果の取得に失敗しました: %w", err)
	}

	if rows == 0 {
		return fmt.Errorf("商品が見つかりません: %s", product.ProdCode)
	}

	return nil
}

// Delete は商品を削除します
func (r *APIProductRepositoryImpl) Delete(ctx context.Context, tx *database.Tx, prodCode string) error {
	query := `DELETE FROM "商品マスタ" WHERE "商品コード" = $1`

	result, err := tx.ExecContext(ctx, query, prodCode)
	if err != nil {
		return fmt.Errorf("商品の削除に失敗しました: %w", err)
	}

	rows, err := result.RowsAffected()
	if err != nil {
		return fmt.Errorf("削除結果の取得に失敗しました: %w", err)
	}

	if rows == 0 {
		return fmt.Errorf("商品が見つかりません: %s", prodCode)
	}

	return nil
}
