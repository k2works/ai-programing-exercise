package handler

import (
	"bytes"
	"encoding/json"
	"net/http"
	"net/http/httptest"
	"testing"

	"github.com/gin-gonic/gin"
	"github.com/k2works/sales-management-db/internal/api/schema"
	"github.com/k2works/sales-management-db/internal/api/service"
	"github.com/k2works/sales-management-db/internal/model"
	"github.com/k2works/sales-management-db/internal/repository"
	"github.com/k2works/sales-management-db/pkg/testutil"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// ヘルパー関数: 文字列をポインターに変換
func strPtr(s string) *string {
	return &s
}

// ヘルパー関数: intをポインターに変換
func intPtr(i int) *int {
	return &i
}

func setupRouter(t *testing.T) (*gin.Engine, *testutil.TestDB) {
	t.Helper()

	// テストモードに設定
	gin.SetMode(gin.TestMode)

	// テスト用 DB のセットアップ
	testDB := testutil.SetupTestDB(t)

	// リポジトリとサービスの初期化
	productRepo := repository.NewProductRepository(testDB.DB)
	productService := service.NewProductServiceV2(productRepo)
	productHandler := NewProductHandlerV2(productService, testDB.DB)

	// ルーターのセットアップ
	router := gin.New()
	v1 := router.Group("/api/v1")
	{
		products := v1.Group("/products")
		{
			products.POST("", productHandler.CreateProduct)
			products.GET("", productHandler.GetAllProducts)
			products.GET("/:prodCode", productHandler.GetProduct)
			products.PUT("/:prodCode", productHandler.UpdateProduct)
			products.DELETE("/:prodCode", productHandler.DeleteProduct)
		}
	}

	return router, testDB
}

func createTestProductCategory(t *testing.T, db *testutil.TestDB) {
	t.Helper()

	categoryRepo := repository.NewProductCategoryRepository(db.DB)
	category := &model.ProductCategory{
		CategoryCode:  "CAT001",
		CategoryName:  "肉類",
		CategoryLevel: 1,
		CategoryPath:  "/肉類/CAT001/",
		IsLowestLevel: 1,
	}
	err := categoryRepo.Create(category)
	require.NoError(t, err)
}

func TestProductHandlerV2_CreateProduct(t *testing.T) {
	t.Run("商品を作成できる", func(t *testing.T) {
		// Setup
		router, testDB := setupRouter(t)
		createTestProductCategory(t, testDB)

		// Request
		request := schema.CreateProductRequest{
			ProdCode:     "PROD00001",
			FullName:     "黒毛和牛サーロインステーキ 200g",
			Name:         "サーロイン",
			Kana:         "クロゲワギュウサーロイン",
			UnitPrice:    5000,
			PoPrice:      3500,
			SupCode:      "COMP0011",
			CategoryCode: "CAT001",
		}

		jsonBody, _ := json.Marshal(request)
		req := httptest.NewRequest(http.MethodPost, "/api/v1/products", bytes.NewBuffer(jsonBody))
		req.Header.Set("Content-Type", "application/json")
		w := httptest.NewRecorder()

		// Execute
		router.ServeHTTP(w, req)

		// Assert
		assert.Equal(t, http.StatusCreated, w.Code)

		var response schema.ProductResponse
		err := json.Unmarshal(w.Body.Bytes(), &response)
		require.NoError(t, err)

		assert.Equal(t, "PROD00001", response.ProdCode)
		assert.Equal(t, "サーロイン", response.Name)
		assert.Equal(t, 5000, response.UnitPrice)
	})

	t.Run("販売単価が仕入単価より低い場合はエラー", func(t *testing.T) {
		// Setup
		router, testDB := setupRouter(t)
		createTestProductCategory(t, testDB)

		// Request with invalid price
		request := schema.CreateProductRequest{
			ProdCode:     "PROD00002",
			FullName:     "商品名",
			Name:         "略称",
			Kana:         "カナ",
			UnitPrice:    1000, // 仕入単価より低い
			PoPrice:      2000,
			SupCode:      "COMP0011",
			CategoryCode: "CAT001",
		}

		jsonBody, _ := json.Marshal(request)
		req := httptest.NewRequest(http.MethodPost, "/api/v1/products", bytes.NewBuffer(jsonBody))
		req.Header.Set("Content-Type", "application/json")
		w := httptest.NewRecorder()

		// Execute
		router.ServeHTTP(w, req)

		// Assert
		assert.Equal(t, http.StatusBadRequest, w.Code)

		var errorResponse schema.ErrorResponse
		err := json.Unmarshal(w.Body.Bytes(), &errorResponse)
		require.NoError(t, err)

		assert.Contains(t, errorResponse.Error, "販売単価が仕入価格より低い")
	})

	t.Run("必須フィールドが不足している場合はバリデーションエラー", func(t *testing.T) {
		// Setup
		router, _ := setupRouter(t)

		// Request with missing required fields
		request := map[string]interface{}{
			"prodCode": "PROD00003",
			// fullName, name, kana などが不足
		}

		jsonBody, _ := json.Marshal(request)
		req := httptest.NewRequest(http.MethodPost, "/api/v1/products", bytes.NewBuffer(jsonBody))
		req.Header.Set("Content-Type", "application/json")
		w := httptest.NewRecorder()

		// Execute
		router.ServeHTTP(w, req)

		// Assert
		assert.Equal(t, http.StatusBadRequest, w.Code)
	})
}

func TestProductHandlerV2_GetAllProducts(t *testing.T) {
	t.Run("商品一覧を取得できる", func(t *testing.T) {
		// Setup
		router, testDB := setupRouter(t)
		createTestProductCategory(t, testDB)

		// Create test products
		productRepo := repository.NewProductRepository(testDB.DB)
		products := []*model.Product{
			{
				ProductCode:         "PROD00001",
				ProductFullName:     "商品1",
				ProductAbbreviation: "商品1",
				ProductNameKana:     "ショウヒン1",
				ProductType:         "1",
				SellingPrice:        1000,
				PurchasePrice:       700,
				ProductCategoryCode: "CAT001",
			},
			{
				ProductCode:         "PROD00002",
				ProductFullName:     "商品2",
				ProductAbbreviation: "商品2",
				ProductNameKana:     "ショウヒン2",
				ProductType:         "1",
				SellingPrice:        2000,
				PurchasePrice:       1400,
				ProductCategoryCode: "CAT001",
			},
		}

		for _, product := range products {
			err := productRepo.Create(product)
			require.NoError(t, err)
		}

		// Request
		req := httptest.NewRequest(http.MethodGet, "/api/v1/products", nil)
		w := httptest.NewRecorder()

		// Execute
		router.ServeHTTP(w, req)

		// Assert
		assert.Equal(t, http.StatusOK, w.Code)

		var response []schema.ProductResponse
		err := json.Unmarshal(w.Body.Bytes(), &response)
		require.NoError(t, err)

		assert.Len(t, response, 2)
		assert.Equal(t, "PROD00001", response[0].ProdCode)
		assert.Equal(t, "PROD00002", response[1].ProdCode)
	})
}

func TestProductHandlerV2_GetProduct(t *testing.T) {
	t.Run("商品詳細を取得できる", func(t *testing.T) {
		// Setup
		router, testDB := setupRouter(t)
		createTestProductCategory(t, testDB)

		// Create test product
		productRepo := repository.NewProductRepository(testDB.DB)
		product := &model.Product{
			ProductCode:         "PROD00001",
			ProductFullName:     "黒毛和牛サーロイン",
			ProductAbbreviation: "サーロイン",
			ProductNameKana:     "クロゲワギュウサーロイン",
			ProductType:         "1",
			SellingPrice:        5000,
			PurchasePrice:       3500,
			ProductCategoryCode: "CAT001",
		}
		err := productRepo.Create(product)
		require.NoError(t, err)

		// Request
		req := httptest.NewRequest(http.MethodGet, "/api/v1/products/PROD00001", nil)
		w := httptest.NewRecorder()

		// Execute
		router.ServeHTTP(w, req)

		// Assert
		assert.Equal(t, http.StatusOK, w.Code)

		var response schema.ProductResponse
		err = json.Unmarshal(w.Body.Bytes(), &response)
		require.NoError(t, err)

		assert.Equal(t, "PROD00001", response.ProdCode)
		assert.Equal(t, "サーロイン", response.Name)
		assert.Equal(t, 5000, response.UnitPrice)
	})

	t.Run("存在しない商品の場合は404エラー", func(t *testing.T) {
		// Setup
		router, _ := setupRouter(t)

		// Request
		req := httptest.NewRequest(http.MethodGet, "/api/v1/products/NOTEXIST", nil)
		w := httptest.NewRecorder()

		// Execute
		router.ServeHTTP(w, req)

		// Assert
		assert.Equal(t, http.StatusNotFound, w.Code)
	})
}

func TestProductHandlerV2_UpdateProduct(t *testing.T) {
	t.Run("商品を更新できる", func(t *testing.T) {
		// Setup
		router, testDB := setupRouter(t)
		createTestProductCategory(t, testDB)

		// Create test product
		productRepo := repository.NewProductRepository(testDB.DB)
		product := &model.Product{
			ProductCode:         "PROD00001",
			ProductFullName:     "元の商品名",
			ProductAbbreviation: "元の略称",
			ProductNameKana:     "モトノリャクショウ",
			ProductType:         "1",
			SellingPrice:        1000,
			PurchasePrice:       700,
			ProductCategoryCode: "CAT001",
		}
		err := productRepo.Create(product)
		require.NoError(t, err)

		// Update request
		updateRequest := schema.UpdateProductRequest{
			FullName:     strPtr("更新後の商品名"),
			Name:         strPtr("更新後"),
			Kana:         strPtr("コウシンゴ"),
			UnitPrice:    intPtr(2000),
			PoPrice:      intPtr(1400),
			SupCode:      strPtr("COMP0011"),
			CategoryCode: strPtr("CAT001"),
		}

		jsonBody, _ := json.Marshal(updateRequest)
		req := httptest.NewRequest(http.MethodPut, "/api/v1/products/PROD00001", bytes.NewBuffer(jsonBody))
		req.Header.Set("Content-Type", "application/json")
		w := httptest.NewRecorder()

		// Execute
		router.ServeHTTP(w, req)

		// Assert
		assert.Equal(t, http.StatusOK, w.Code)

		var response schema.ProductResponse
		err = json.Unmarshal(w.Body.Bytes(), &response)
		require.NoError(t, err)

		assert.Equal(t, "PROD00001", response.ProdCode)
		assert.Equal(t, "更新後", response.Name)
		assert.Equal(t, 2000, response.UnitPrice)
	})

	t.Run("存在しない商品を更新しようとすると404エラー", func(t *testing.T) {
		// Setup
		router, _ := setupRouter(t)

		// Update request for non-existent product
		updateRequest := schema.UpdateProductRequest{
			FullName:     strPtr("商品名"),
			Name:         strPtr("略称"),
			Kana:         strPtr("カナ"),
			UnitPrice:    intPtr(1000),
			PoPrice:      intPtr(700),
			SupCode:      strPtr("COMP0011"),
			CategoryCode: strPtr("CAT001"),
		}

		jsonBody, _ := json.Marshal(updateRequest)
		req := httptest.NewRequest(http.MethodPut, "/api/v1/products/NOTEXIST", bytes.NewBuffer(jsonBody))
		req.Header.Set("Content-Type", "application/json")
		w := httptest.NewRecorder()

		// Execute
		router.ServeHTTP(w, req)

		// Assert
		assert.Equal(t, http.StatusNotFound, w.Code)
	})
}

func TestProductHandlerV2_DeleteProduct(t *testing.T) {
	t.Run("商品を削除できる", func(t *testing.T) {
		// Setup
		router, testDB := setupRouter(t)
		createTestProductCategory(t, testDB)

		// Create test product
		productRepo := repository.NewProductRepository(testDB.DB)
		product := &model.Product{
			ProductCode:         "PROD00001",
			ProductFullName:     "削除対象商品",
			ProductAbbreviation: "削除商品",
			ProductNameKana:     "サクジョショウヒン",
			ProductType:         "1",
			SellingPrice:        1000,
			PurchasePrice:       700,
			ProductCategoryCode: "CAT001",
		}
		err := productRepo.Create(product)
		require.NoError(t, err)

		// Delete request
		req := httptest.NewRequest(http.MethodDelete, "/api/v1/products/PROD00001", nil)
		w := httptest.NewRecorder()

		// Execute
		router.ServeHTTP(w, req)

		// Assert
		assert.Equal(t, http.StatusNoContent, w.Code)

		// Verify product was deleted
		deleted, err := productRepo.FindByID("PROD00001")
		require.NoError(t, err)
		assert.Nil(t, deleted)
	})

	t.Run("存在しない商品を削除しようとすると404エラー", func(t *testing.T) {
		// Setup
		router, _ := setupRouter(t)

		// Delete request for non-existent product
		req := httptest.NewRequest(http.MethodDelete, "/api/v1/products/NOTEXIST", nil)
		w := httptest.NewRecorder()

		// Execute
		router.ServeHTTP(w, req)

		// Assert
		assert.Equal(t, http.StatusNotFound, w.Code)
	})
}
