package repository

import (
	"testing"
	"time"

	"github.com/k2works/sales-management-db/internal/model"
	"github.com/k2works/sales-management-db/pkg/testutil"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestPriceByCustomerRepository_Create(t *testing.T) {
	testDB := testutil.SetupTestDB(t)

	t.Run("顧客別単価を登録できる", func(t *testing.T) {
		// Given: 商品分類と商品が存在する
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

		productRepo := NewProductRepository(testDB.DB)
		product := &model.Product{
			ProductCode:         "PROD001",
			ProductFullName:     "ノートパソコン ThinkPad X1 Carbon",
			ProductAbbreviation: "ThinkPad",
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
		err = productRepo.Create(product)
		require.NoError(t, err)

		// When: 顧客別単価を登録すると
		repo := NewPriceByCustomerRepository(testDB.DB)
		price := &model.PriceByCustomer{
			ProductCode:  "PROD001",
			CustomerCode: "CUST001",
			Price:        180000,
			CreatedAt:    time.Now(),
			CreatedBy:    "admin",
			UpdatedAt:    time.Now(),
			UpdatedBy:    "admin",
		}
		err = repo.Create(price)

		// Then: エラーなく登録できる
		require.NoError(t, err)

		// 登録された顧客別単価を確認
		found, err := repo.FindByID("PROD001", "CUST001")
		require.NoError(t, err)
		assert.Equal(t, "PROD001", found.ProductCode)
		assert.Equal(t, "CUST001", found.CustomerCode)
		assert.Equal(t, 180000, found.Price)
	})
}

func TestPriceByCustomerRepository_FindByProduct(t *testing.T) {
	testDB := testutil.SetupTestDB(t)

	t.Run("商品コードで顧客別単価を取得できる", func(t *testing.T) {
		// Given: 商品分類、商品、複数の顧客別単価が登録されている
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

		productRepo := NewProductRepository(testDB.DB)
		product := &model.Product{
			ProductCode:         "PROD001",
			ProductFullName:     "ノートパソコン ThinkPad X1 Carbon",
			ProductAbbreviation: "ThinkPad",
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
		err = productRepo.Create(product)
		require.NoError(t, err)

		repo := NewPriceByCustomerRepository(testDB.DB)
		prices := []*model.PriceByCustomer{
			{
				ProductCode:  "PROD001",
				CustomerCode: "CUST001",
				Price:        180000,
				CreatedAt:    time.Now(),
				CreatedBy:    "admin",
				UpdatedAt:    time.Now(),
				UpdatedBy:    "admin",
			},
			{
				ProductCode:  "PROD001",
				CustomerCode: "CUST002",
				Price:        170000,
				CreatedAt:    time.Now(),
				CreatedBy:    "admin",
				UpdatedAt:    time.Now(),
				UpdatedBy:    "admin",
			},
		}

		for _, p := range prices {
			err := repo.Create(p)
			require.NoError(t, err)
		}

		// When: 商品コードで顧客別単価を取得すると
		found, err := repo.FindByProduct("PROD001")

		// Then: 登録した顧客別単価がすべて取得できる
		require.NoError(t, err)
		assert.Len(t, found, 2)
	})
}

func TestPriceByCustomerRepository_Update(t *testing.T) {
	testDB := testutil.SetupTestDB(t)

	t.Run("顧客別単価を更新できる", func(t *testing.T) {
		// Given: 商品分類、商品、顧客別単価が登録されている
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

		productRepo := NewProductRepository(testDB.DB)
		product := &model.Product{
			ProductCode:         "PROD001",
			ProductFullName:     "ノートパソコン ThinkPad X1 Carbon",
			ProductAbbreviation: "ThinkPad",
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
		err = productRepo.Create(product)
		require.NoError(t, err)

		repo := NewPriceByCustomerRepository(testDB.DB)
		price := &model.PriceByCustomer{
			ProductCode:  "PROD001",
			CustomerCode: "CUST001",
			Price:        180000,
			CreatedAt:    time.Now(),
			CreatedBy:    "admin",
			UpdatedAt:    time.Now(),
			UpdatedBy:    "admin",
		}
		err = repo.Create(price)
		require.NoError(t, err)

		// When: 顧客別単価を更新すると
		price.Price = 175000
		price.UpdatedAt = time.Now()
		price.UpdatedBy = "user1"
		err = repo.Update(price)

		// Then: 更新が反映される
		require.NoError(t, err)

		found, err := repo.FindByID("PROD001", "CUST001")
		require.NoError(t, err)
		assert.Equal(t, 175000, found.Price)
		assert.Equal(t, "user1", found.UpdatedBy)
	})
}

func TestPriceByCustomerRepository_Delete(t *testing.T) {
	testDB := testutil.SetupTestDB(t)

	t.Run("顧客別単価を削除できる", func(t *testing.T) {
		// Given: 商品分類、商品、顧客別単価が登録されている
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

		productRepo := NewProductRepository(testDB.DB)
		product := &model.Product{
			ProductCode:         "PROD001",
			ProductFullName:     "ノートパソコン ThinkPad X1 Carbon",
			ProductAbbreviation: "ThinkPad",
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
		err = productRepo.Create(product)
		require.NoError(t, err)

		repo := NewPriceByCustomerRepository(testDB.DB)
		price := &model.PriceByCustomer{
			ProductCode:  "PROD001",
			CustomerCode: "CUST001",
			Price:        180000,
			CreatedAt:    time.Now(),
			CreatedBy:    "admin",
			UpdatedAt:    time.Now(),
			UpdatedBy:    "admin",
		}
		err = repo.Create(price)
		require.NoError(t, err)

		// When: 顧客別単価を削除すると
		err = repo.Delete("PROD001", "CUST001")

		// Then: 削除される
		require.NoError(t, err)

		found, err := repo.FindByID("PROD001", "CUST001")
		require.NoError(t, err)
		assert.Nil(t, found)
	})
}
