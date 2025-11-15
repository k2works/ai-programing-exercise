package service

import (
	"context"
	"testing"
	"time"

	"github.com/k2works/sales-management-db/internal/api/schema"
	"github.com/k2works/sales-management-db/internal/domain"
	"github.com/k2works/sales-management-db/pkg/database"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/mock"
)

// MockProductRepository は ProductRepository のモック
type MockProductRepository struct {
	mock.Mock
}

func (m *MockProductRepository) Create(ctx context.Context, tx *database.Tx, product *domain.Product) error {
	args := m.Called(ctx, tx, product)
	return args.Error(0)
}

func (m *MockProductRepository) FindByID(ctx context.Context, db database.Queryer, prodCode string) (*domain.Product, error) {
	args := m.Called(ctx, db, prodCode)
	if args.Get(0) == nil {
		return nil, args.Error(1)
	}
	return args.Get(0).(*domain.Product), args.Error(1)
}

func (m *MockProductRepository) FindAll(ctx context.Context, db database.Queryer) ([]*domain.Product, error) {
	args := m.Called(ctx, db)
	return args.Get(0).([]*domain.Product), args.Error(1)
}

func (m *MockProductRepository) Update(ctx context.Context, tx *database.Tx, product *domain.Product) error {
	args := m.Called(ctx, tx, product)
	return args.Error(0)
}

func (m *MockProductRepository) Delete(ctx context.Context, tx *database.Tx, prodCode string) error {
	args := m.Called(ctx, tx, prodCode)
	return args.Error(0)
}

func TestProductService_CreateProduct(t *testing.T) {
	mockRepo := new(MockProductRepository)
	service := NewProductService(mockRepo)
	ctx := context.Background()

	request := schema.CreateProductRequest{
		ProdCode:  "TEST00001",
		FullName:  "テスト商品フルネーム",
		Name:      "テスト商品",
		Kana:      strPtr("テストショウヒン"),
		UnitPrice: 1000,
		PoPrice:   700,
		SupCode:   "COMP0011",
	}

	now := time.Now()
	createdProduct := &domain.Product{
		ProdCode:   request.ProdCode,
		FullName:   request.FullName,
		Name:       request.Name,
		Kana:       request.Kana,
		UnitPrice:  request.UnitPrice,
		PoPrice:    request.PoPrice,
		SupCode:    &request.SupCode,
		CreateDate: now,
		Creator:    strPtr("api"),
		UpdateDate: now,
		Updater:    strPtr("api"),
	}

	// モックの設定
	mockRepo.On("Create", ctx, mock.Anything, mock.Anything).Return(nil)
	mockRepo.On("FindByID", ctx, mock.Anything, "TEST00001").Return(createdProduct, nil)

	// 実行
	result, err := service.CreateProduct(ctx, nil, request)

	// 検証
	assert.NoError(t, err)
	assert.NotNil(t, result)
	assert.Equal(t, "TEST00001", result.ProdCode)
	assert.Equal(t, 1000, result.UnitPrice)
	mockRepo.AssertExpectations(t)
}

func TestProductService_CreateProduct_ValidationError(t *testing.T) {
	mockRepo := new(MockProductRepository)
	service := NewProductService(mockRepo)
	ctx := context.Background()

	request := schema.CreateProductRequest{
		ProdCode:  "TEST00001",
		FullName:  "テスト商品フルネーム",
		Name:      "テスト商品",
		Kana:      strPtr("テストショウヒン"),
		UnitPrice: 500, // 原価より低い
		PoPrice:   700,
		SupCode:   "COMP0011",
	}

	// 実行
	_, err := service.CreateProduct(ctx, nil, request)

	// 検証
	assert.Error(t, err)
	assert.Contains(t, err.Error(), "販売単価が仕入価格より低い")
}

func TestProductService_GetAllProducts(t *testing.T) {
	mockRepo := new(MockProductRepository)
	service := NewProductService(mockRepo)
	ctx := context.Background()

	now := time.Now()
	products := []*domain.Product{
		{
			ProdCode:   "TEST00001",
			FullName:   "商品1",
			Name:       "商品1",
			Kana:       strPtr("ショウヒン1"),
			UnitPrice:  1000,
			PoPrice:    700,
			SupCode:    strPtr("COMP0011"),
			CreateDate: now,
			Creator:    strPtr("test"),
			UpdateDate: now,
			Updater:    strPtr("test"),
		},
		{
			ProdCode:   "TEST00002",
			FullName:   "商品2",
			Name:       "商品2",
			Kana:       strPtr("ショウヒン2"),
			UnitPrice:  2000,
			PoPrice:    1400,
			SupCode:    strPtr("COMP0011"),
			CreateDate: now,
			Creator:    strPtr("test"),
			UpdateDate: now,
			Updater:    strPtr("test"),
		},
	}

	mockRepo.On("FindAll", ctx, mock.Anything).Return(products, nil)

	// 実行
	result, err := service.GetAllProducts(ctx, nil)

	// 検証
	assert.NoError(t, err)
	assert.Len(t, result, 2)
	mockRepo.AssertExpectations(t)
}
