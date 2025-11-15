package repository

import (
	"context"

	"github.com/k2works/sales-management-db/internal/domain"
	"github.com/k2works/sales-management-db/pkg/database"
)

// APIProductRepository は API レイヤー用の商品リポジトリインターフェースです
type APIProductRepository interface {
	Create(ctx context.Context, tx *database.Tx, product *domain.Product) error
	FindByID(ctx context.Context, db database.Queryer, prodCode string) (*domain.Product, error)
	FindAll(ctx context.Context, db database.Queryer) ([]*domain.Product, error)
	Update(ctx context.Context, tx *database.Tx, product *domain.Product) error
	Delete(ctx context.Context, tx *database.Tx, prodCode string) error
}
