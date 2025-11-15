package repository

import (
	"testing"
	"time"

	"github.com/k2works/sales-management-db/internal/model"
	"github.com/k2works/sales-management-db/pkg/testutil"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestProductRepository_Create(t *testing.T) {
	testDB := testutil.SetupTestDB(t)

	t.Run("商品を登録できる", func(t *testing.T) {
		// Given: 商品分類が存在する
		categoryRepo := NewProductCategoryRepository(testDB.DB)
		category := &model.ProductCategory{
			CategoryCode:  "PC001",
			CategoryName:  "ノートPC",
			CategoryLevel: 1,
			CategoryPath:  "/PC/PC001/",
			IsLowestLevel: 1,
			CreatedAt:     time.Now(),
			CreatedBy:     "admin",
			UpdatedAt:     time.Now(),
			UpdatedBy:     "admin",
		}
		err := categoryRepo.Create(category)
		require.NoError(t, err)

		// When: 新しい商品を作成
		repo := NewProductRepository(testDB.DB)
		product := &model.Product{
			ProductCode:         "PROD001",
			ProductFullName:     "ノートパソコン ThinkPad X1 Carbon",
			ProductAbbreviation: "ThinkPad",
			ProductNameKana:     "ノートパソコン シンクパッド エックスワン カーボン",
			ProductType:         "PC",
			ModelNumber:         "20XW0000JP",
			SellingPrice:        200000,
			PurchasePrice:       150000,
			CostOfSales:         150000,
			TaxCategory:         1,
			ProductCategoryCode: "PC001",
			InventoryManaged:    1,
			CreatedAt:           time.Now(),
			CreatedBy:           "admin",
			UpdatedAt:           time.Now(),
			UpdatedBy:           "admin",
		}

		// Then: 商品を登録すると
		err = repo.Create(product)

		// Then: エラーなく登録できる
		require.NoError(t, err)

		// 登録された商品を確認
		found, err := repo.FindByID("PROD001")
		require.NoError(t, err)
		assert.Equal(t, "PROD001", found.ProductCode)
		assert.Equal(t, "ThinkPad", found.ProductAbbreviation)
		assert.Equal(t, 200000, found.SellingPrice)
	})
}

func TestProductRepository_FindAll(t *testing.T) {
	testDB := testutil.SetupTestDB(t)

	t.Run("すべての商品を取得できる", func(t *testing.T) {
		// Given: 商品分類が存在し、複数の商品が登録されている
		categoryRepo := NewProductCategoryRepository(testDB.DB)
		category := &model.ProductCategory{
			CategoryCode:  "PC001",
			CategoryName:  "ノートPC",
			CategoryLevel: 1,
			CategoryPath:  "/PC/PC001/",
			IsLowestLevel: 1,
			CreatedAt:     time.Now(),
			CreatedBy:     "admin",
			UpdatedAt:     time.Now(),
			UpdatedBy:     "admin",
		}
		err := categoryRepo.Create(category)
		require.NoError(t, err)

		repo := NewProductRepository(testDB.DB)
		products := []*model.Product{
			{
				ProductCode:         "PROD001",
				ProductFullName:     "ノートパソコン ThinkPad X1 Carbon",
				ProductAbbreviation: "ThinkPad",
				ProductNameKana:     "ノートパソコン シンクパッド",
				ProductCategoryCode: "PC001",
				SellingPrice:        200000,
				PurchasePrice:       150000,
				CostOfSales:         150000,
				TaxCategory:         1,
				InventoryManaged:    1,
				CreatedAt:           time.Now(),
				CreatedBy:           "admin",
				UpdatedAt:           time.Now(),
				UpdatedBy:           "admin",
			},
			{
				ProductCode:         "PROD002",
				ProductFullName:     "デスクトップパソコン OptiPlex 7090",
				ProductAbbreviation: "OptiPlex",
				ProductNameKana:     "デスクトップパソコン オプティプレックス",
				ProductCategoryCode: "PC001",
				SellingPrice:        150000,
				PurchasePrice:       100000,
				CostOfSales:         100000,
				TaxCategory:         1,
				InventoryManaged:    1,
				CreatedAt:           time.Now(),
				CreatedBy:           "admin",
				UpdatedAt:           time.Now(),
				UpdatedBy:           "admin",
			},
		}

		for _, p := range products {
			err := repo.Create(p)
			require.NoError(t, err)
		}

		// When: すべての商品を取得すると
		found, err := repo.FindAll()

		// Then: 登録した商品がすべて取得できる
		require.NoError(t, err)
		assert.Len(t, found, 2)
	})
}

func TestProductRepository_Update(t *testing.T) {
	testDB := testutil.SetupTestDB(t)

	t.Run("商品を更新できる", func(t *testing.T) {
		// Given: 商品分類が存在する
		categoryRepo := NewProductCategoryRepository(testDB.DB)
		category := &model.ProductCategory{
			CategoryCode:  "PC001",
			CategoryName:  "ノートPC",
			CategoryLevel: 1,
			CategoryPath:  "/PC/PC001/",
			IsLowestLevel: 1,
			CreatedAt:     time.Now(),
			CreatedBy:     "admin",
			UpdatedAt:     time.Now(),
			UpdatedBy:     "admin",
		}
		err := categoryRepo.Create(category)
		require.NoError(t, err)

		// Given: 商品が登録されている
		repo := NewProductRepository(testDB.DB)
		product := &model.Product{
			ProductCode:         "PROD001",
			ProductFullName:     "ノートパソコン ThinkPad X1 Carbon",
			ProductAbbreviation: "ThinkPad",
			ProductNameKana:     "ノートパソコン シンクパッド",
			ProductCategoryCode: "PC001",
			SellingPrice:        200000,
			PurchasePrice:       150000,
			CostOfSales:         150000,
			TaxCategory:         1,
			InventoryManaged:    1,
			CreatedAt:           time.Now(),
			CreatedBy:           "admin",
			UpdatedAt:           time.Now(),
			UpdatedBy:           "admin",
		}
		err = repo.Create(product)
		require.NoError(t, err)

		// When: 商品情報を更新すると
		product.SellingPrice = 180000
		product.UpdatedAt = time.Now()
		product.UpdatedBy = "user1"
		err = repo.Update(product)

		// Then: 更新が反映される
		require.NoError(t, err)

		found, err := repo.FindByID("PROD001")
		require.NoError(t, err)
		assert.Equal(t, 180000, found.SellingPrice)
		assert.Equal(t, "user1", found.UpdatedBy)
	})
}

func TestProductRepository_Delete(t *testing.T) {
	testDB := testutil.SetupTestDB(t)

	t.Run("商品を削除できる", func(t *testing.T) {
		// Given: 商品分類が存在する
		categoryRepo := NewProductCategoryRepository(testDB.DB)
		category := &model.ProductCategory{
			CategoryCode:  "PC001",
			CategoryName:  "ノートPC",
			CategoryLevel: 1,
			CategoryPath:  "/PC/PC001/",
			IsLowestLevel: 1,
			CreatedAt:     time.Now(),
			CreatedBy:     "admin",
			UpdatedAt:     time.Now(),
			UpdatedBy:     "admin",
		}
		err := categoryRepo.Create(category)
		require.NoError(t, err)

		// Given: 商品が登録されている
		repo := NewProductRepository(testDB.DB)
		product := &model.Product{
			ProductCode:         "PROD001",
			ProductFullName:     "ノートパソコン ThinkPad X1 Carbon",
			ProductAbbreviation: "ThinkPad",
			ProductNameKana:     "ノートパソコン シンクパッド",
			ProductCategoryCode: "PC001",
			SellingPrice:        200000,
			PurchasePrice:       150000,
			CostOfSales:         150000,
			TaxCategory:         1,
			InventoryManaged:    1,
			CreatedAt:           time.Now(),
			CreatedBy:           "admin",
			UpdatedAt:           time.Now(),
			UpdatedBy:           "admin",
		}
		err = repo.Create(product)
		require.NoError(t, err)

		// When: 商品を削除すると
		err = repo.Delete("PROD001")

		// Then: 削除される
		require.NoError(t, err)

		found, err := repo.FindByID("PROD001")
		require.NoError(t, err)
		assert.Nil(t, found)
	})
}
