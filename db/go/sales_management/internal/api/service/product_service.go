package service

import (
	"context"
	"errors"
	"time"

	"github.com/k2works/sales-management-db/internal/api/schema"
	"github.com/k2works/sales-management-db/internal/domain"
	"github.com/k2works/sales-management-db/internal/repository"
	"github.com/k2works/sales-management-db/pkg/database"
)

// ProductService 商品サービス
type ProductService struct {
	repo repository.APIProductRepository
}

// NewProductService 商品サービスを作成
func NewProductService(repo repository.APIProductRepository) *ProductService {
	return &ProductService{repo: repo}
}

// CreateProduct 商品を作成
func (s *ProductService) CreateProduct(ctx context.Context, tx *database.Tx, request schema.CreateProductRequest) (*schema.ProductResponse, error) {
	// ビジネスルールの適用：単価が原価より安い場合はエラー
	if request.UnitPrice < request.PoPrice {
		return nil, errors.New("販売単価が仕入価格より低い設定はできません")
	}

	now := time.Now()
	product := &domain.Product{
		ProdCode:     request.ProdCode,
		FullName:     request.FullName,
		Name:         request.Name,
		Kana:         request.Kana,
		UnitPrice:    request.UnitPrice,
		PoPrice:      request.PoPrice,
		SupCode:      &request.SupCode,
		CategoryCode: request.CategoryCode,
		CreateDate:   now,
		Creator:      strPtr("api"),
		UpdateDate:   now,
		Updater:      strPtr("api"),
	}

	err := s.repo.Create(ctx, tx, product)
	if err != nil {
		return nil, err
	}

	created, err := s.repo.FindByID(ctx, tx, request.ProdCode)
	if err != nil {
		return nil, err
	}

	if created == nil {
		return nil, errors.New("商品の作成に失敗しました")
	}

	response := schema.ProductResponseFromDomain(created)
	return &response, nil
}

// GetAllProducts すべての商品を取得
func (s *ProductService) GetAllProducts(ctx context.Context, db database.Queryer) ([]schema.ProductResponse, error) {
	products, err := s.repo.FindAll(ctx, db)
	if err != nil {
		return nil, err
	}

	responses := make([]schema.ProductResponse, len(products))
	for i, p := range products {
		responses[i] = schema.ProductResponseFromDomain(p)
	}

	return responses, nil
}

// GetProductByCode 商品コードで商品を取得
func (s *ProductService) GetProductByCode(ctx context.Context, db database.Queryer, prodCode string) (*schema.ProductResponse, error) {
	product, err := s.repo.FindByID(ctx, db, prodCode)
	if err != nil {
		return nil, err
	}

	if product == nil {
		return nil, nil
	}

	response := schema.ProductResponseFromDomain(product)
	return &response, nil
}

// UpdateProduct 商品を更新
func (s *ProductService) UpdateProduct(ctx context.Context, tx *database.Tx, prodCode string, request schema.UpdateProductRequest) (*schema.ProductResponse, error) {
	existing, err := s.repo.FindByID(ctx, tx, prodCode)
	if err != nil {
		return nil, err
	}

	if existing == nil {
		return nil, errors.New("商品が見つかりません")
	}

	// 更新内容を反映
	if request.FullName != nil {
		existing.FullName = *request.FullName
	}
	if request.Name != nil {
		existing.Name = *request.Name
	}
	if request.Kana != nil {
		existing.Kana = request.Kana
	}
	if request.UnitPrice != nil {
		existing.UnitPrice = *request.UnitPrice
	}
	if request.PoPrice != nil {
		existing.PoPrice = *request.PoPrice
	}
	if request.SupCode != nil {
		existing.SupCode = request.SupCode
	}
	if request.CategoryCode != nil {
		existing.CategoryCode = request.CategoryCode
	}

	existing.UpdateDate = time.Now()
	existing.Updater = strPtr("api")

	// ビジネスルールの適用
	if existing.UnitPrice < existing.PoPrice {
		return nil, errors.New("販売単価が仕入価格より低い設定はできません")
	}

	err = s.repo.Update(ctx, tx, existing)
	if err != nil {
		return nil, err
	}

	response := schema.ProductResponseFromDomain(existing)
	return &response, nil
}

// DeleteProduct 商品を削除
func (s *ProductService) DeleteProduct(ctx context.Context, tx *database.Tx, prodCode string) error {
	existing, err := s.repo.FindByID(ctx, tx, prodCode)
	if err != nil {
		return err
	}

	if existing == nil {
		return errors.New("商品が見つかりません")
	}

	return s.repo.Delete(ctx, tx, prodCode)
}

func strPtr(s string) *string {
	return &s
}
