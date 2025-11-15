package service

import (
	"context"
	"errors"
	"time"

	"github.com/k2works/sales-management-db/internal/api/schema"
	"github.com/k2works/sales-management-db/internal/model"
	"github.com/k2works/sales-management-db/internal/repository"
)

// ProductServiceV2 商品サービス（既存リポジトリ使用）
type ProductServiceV2 struct {
	repo *repository.ProductRepository
}

// NewProductServiceV2 商品サービスを作成
func NewProductServiceV2(repo *repository.ProductRepository) *ProductServiceV2 {
	return &ProductServiceV2{repo: repo}
}

// CreateProduct 商品を作成
func (s *ProductServiceV2) CreateProduct(ctx context.Context, exec repository.Execer, request schema.CreateProductRequest) (*schema.ProductResponse, error) {
	// ビジネスルールの適用：単価が原価より安い場合はエラー
	if request.UnitPrice < request.PoPrice {
		return nil, errors.New("販売単価が仕入価格より低い設定はできません")
	}

	now := time.Now()
	product := &model.Product{
		ProductCode:         request.ProdCode,
		ProductFullName:     request.FullName,
		ProductAbbreviation: request.Name,
		ProductNameKana:     request.Kana,
		ProductType:         "1", // デフォルト値
		ModelNumber:         "",  // デフォルト値
		SellingPrice:        request.UnitPrice,
		PurchasePrice:       request.PoPrice,
		CostOfSales:         request.PoPrice, // 仕入単価と同じ
		TaxCategory:         1,                // デフォルト値（課税）
		ProductCategoryCode: request.CategoryCode,
		MiscCategory:        0,  // デフォルト値
		InventoryManaged:    1,  // デフォルト値（在庫管理対象）
		StockAllocation:     1,  // デフォルト値（在庫引当）
		SupplierCode:        request.SupCode,
		SupplierBranch:      1, // デフォルト値
		CreatedAt:           now,
		CreatedBy:           "api",
		UpdatedAt:           now,
		UpdatedBy:           "api",
	}

	err := s.repo.CreateWithContext(ctx, exec, product)
	if err != nil {
		return nil, err
	}

	created, err := s.repo.FindByIDWithContext(ctx, exec, request.ProdCode)
	if err != nil {
		return nil, err
	}

	if created == nil {
		return nil, errors.New("商品の作成に失敗しました")
	}

	response := schema.ProductResponseFromModel(created)
	return &response, nil
}

// GetAllProducts すべての商品を取得
func (s *ProductServiceV2) GetAllProducts(ctx context.Context, exec repository.Execer) ([]schema.ProductResponse, error) {
	products, err := s.repo.FindAllWithContext(ctx, exec)
	if err != nil {
		return nil, err
	}

	responses := make([]schema.ProductResponse, len(products))
	for i, p := range products {
		responses[i] = schema.ProductResponseFromModel(p)
	}

	return responses, nil
}

// GetProductByCode 商品コードで商品を取得
func (s *ProductServiceV2) GetProductByCode(ctx context.Context, exec repository.Execer, prodCode string) (*schema.ProductResponse, error) {
	product, err := s.repo.FindByIDWithContext(ctx, exec, prodCode)
	if err != nil {
		return nil, err
	}

	if product == nil {
		return nil, nil
	}

	response := schema.ProductResponseFromModel(product)
	return &response, nil
}

// UpdateProduct 商品を更新
func (s *ProductServiceV2) UpdateProduct(ctx context.Context, exec repository.Execer, prodCode string, request schema.UpdateProductRequest) (*schema.ProductResponse, error) {
	existing, err := s.repo.FindByIDWithContext(ctx, exec, prodCode)
	if err != nil {
		return nil, err
	}

	if existing == nil {
		return nil, errors.New("商品が見つかりません")
	}

	// 更新内容を反映
	if request.FullName != nil {
		existing.ProductFullName = *request.FullName
	}
	if request.Name != nil {
		existing.ProductAbbreviation = *request.Name
	}
	if request.Kana != nil {
		existing.ProductNameKana = *request.Kana
	}
	if request.UnitPrice != nil {
		existing.SellingPrice = *request.UnitPrice
	}
	if request.PoPrice != nil {
		existing.PurchasePrice = *request.PoPrice
		existing.CostOfSales = *request.PoPrice // 仕入単価と同じ
	}
	if request.SupCode != nil {
		existing.SupplierCode = *request.SupCode
	}
	if request.CategoryCode != nil {
		existing.ProductCategoryCode = *request.CategoryCode
	}

	existing.UpdatedAt = time.Now()
	existing.UpdatedBy = "api"

	// ビジネスルールの適用
	if existing.SellingPrice < existing.PurchasePrice {
		return nil, errors.New("販売単価が仕入価格より低い設定はできません")
	}

	err = s.repo.UpdateWithContext(ctx, exec, existing)
	if err != nil {
		return nil, err
	}

	response := schema.ProductResponseFromModel(existing)
	return &response, nil
}

// DeleteProduct 商品を削除
func (s *ProductServiceV2) DeleteProduct(ctx context.Context, exec repository.Execer, prodCode string) error {
	existing, err := s.repo.FindByIDWithContext(ctx, exec, prodCode)
	if err != nil {
		return err
	}

	if existing == nil {
		return errors.New("商品が見つかりません")
	}

	return s.repo.DeleteWithContext(ctx, exec, prodCode)
}
