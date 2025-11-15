package repository

import (
	"testing"
	"time"

	"github.com/k2works/sales-management-db/internal/model"
	"github.com/k2works/sales-management-db/pkg/testutil"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestAlternateProductRepository_Create(t *testing.T) {
	testDB := testutil.SetupTestDB(t)

	t.Run("代替商品を登録できる", func(t *testing.T) {
		// Given: 商品分類と複数の商品が存在する
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
		products := []*model.Product{
			{
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
			},
			{
				ProductCode:         "PROD002",
				ProductFullName:     "ノートパソコン Let's Note",
				ProductAbbreviation: "LetsNote",
				ProductCategoryCode: "PC001",
				SellingPrice:        180000,
				PurchasePrice:       130000,
				CostOfSales:         130000,
				TaxCategory:         1,
				InventoryManaged:    1,
				CreatedAt:           time.Now(),
				CreatedBy:           "admin",
				UpdatedAt:           time.Now(),
				UpdatedBy:           "admin",
			},
		}
		for _, p := range products {
			err := productRepo.Create(p)
			require.NoError(t, err)
		}

		// When: 代替商品を登録すると
		repo := NewAlternateProductRepository(testDB.DB)
		alternate := &model.AlternateProduct{
			ProductCode:          "PROD001",
			AlternateProductCode: "PROD002",
			Priority:             1,
			CreatedAt:            time.Now(),
			CreatedBy:            "admin",
			UpdatedAt:            time.Now(),
			UpdatedBy:            "admin",
		}
		err = repo.Create(alternate)

		// Then: エラーなく登録できる
		require.NoError(t, err)

		// 登録された代替商品を確認
		found, err := repo.FindByID("PROD001", "PROD002")
		require.NoError(t, err)
		assert.Equal(t, "PROD001", found.ProductCode)
		assert.Equal(t, "PROD002", found.AlternateProductCode)
		assert.Equal(t, 1, found.Priority)
	})
}

func TestAlternateProductRepository_FindByProduct(t *testing.T) {
	testDB := testutil.SetupTestDB(t)

	t.Run("商品コードで代替商品を取得できる", func(t *testing.T) {
		// Given: 商品分類、商品、複数の代替商品が登録されている
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
		products := []*model.Product{
			{
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
			},
			{
				ProductCode:         "PROD002",
				ProductFullName:     "ノートパソコン Let's Note",
				ProductAbbreviation: "LetsNote",
				ProductCategoryCode: "PC001",
				SellingPrice:        180000,
				PurchasePrice:       130000,
				CostOfSales:         130000,
				TaxCategory:         1,
				InventoryManaged:    1,
				CreatedAt:           time.Now(),
				CreatedBy:           "admin",
				UpdatedAt:           time.Now(),
				UpdatedBy:           "admin",
			},
			{
				ProductCode:         "PROD003",
				ProductFullName:     "ノートパソコン VAIO",
				ProductAbbreviation: "VAIO",
				ProductCategoryCode: "PC001",
				SellingPrice:        190000,
				PurchasePrice:       140000,
				CostOfSales:         140000,
				TaxCategory:         1,
				InventoryManaged:    1,
				CreatedAt:           time.Now(),
				CreatedBy:           "admin",
				UpdatedAt:           time.Now(),
				UpdatedBy:           "admin",
			},
		}
		for _, p := range products {
			err := productRepo.Create(p)
			require.NoError(t, err)
		}

		repo := NewAlternateProductRepository(testDB.DB)
		alternates := []*model.AlternateProduct{
			{
				ProductCode:          "PROD001",
				AlternateProductCode: "PROD002",
				Priority:             1,
				CreatedAt:            time.Now(),
				CreatedBy:            "admin",
				UpdatedAt:            time.Now(),
				UpdatedBy:            "admin",
			},
			{
				ProductCode:          "PROD001",
				AlternateProductCode: "PROD003",
				Priority:             2,
				CreatedAt:            time.Now(),
				CreatedBy:            "admin",
				UpdatedAt:            time.Now(),
				UpdatedBy:            "admin",
			},
		}

		for _, a := range alternates {
			err := repo.Create(a)
			require.NoError(t, err)
		}

		// When: 商品コードで代替商品を取得すると
		found, err := repo.FindByProduct("PROD001")

		// Then: 優先順位順で代替商品が取得できる
		require.NoError(t, err)
		assert.Len(t, found, 2)
		assert.Equal(t, "PROD002", found[0].AlternateProductCode)
		assert.Equal(t, 1, found[0].Priority)
		assert.Equal(t, "PROD003", found[1].AlternateProductCode)
		assert.Equal(t, 2, found[1].Priority)
	})
}

func TestAlternateProductRepository_Update(t *testing.T) {
	testDB := testutil.SetupTestDB(t)

	t.Run("代替商品の優先順位を更新できる", func(t *testing.T) {
		// Given: 商品分類、商品、代替商品が登録されている
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
		products := []*model.Product{
			{
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
			},
			{
				ProductCode:         "PROD002",
				ProductFullName:     "ノートパソコン Let's Note",
				ProductAbbreviation: "LetsNote",
				ProductCategoryCode: "PC001",
				SellingPrice:        180000,
				PurchasePrice:       130000,
				CostOfSales:         130000,
				TaxCategory:         1,
				InventoryManaged:    1,
				CreatedAt:           time.Now(),
				CreatedBy:           "admin",
				UpdatedAt:           time.Now(),
				UpdatedBy:           "admin",
			},
		}
		for _, p := range products {
			err := productRepo.Create(p)
			require.NoError(t, err)
		}

		repo := NewAlternateProductRepository(testDB.DB)
		alternate := &model.AlternateProduct{
			ProductCode:          "PROD001",
			AlternateProductCode: "PROD002",
			Priority:             1,
			CreatedAt:            time.Now(),
			CreatedBy:            "admin",
			UpdatedAt:            time.Now(),
			UpdatedBy:            "admin",
		}
		err = repo.Create(alternate)
		require.NoError(t, err)

		// When: 代替商品の優先順位を更新すると
		alternate.Priority = 2
		alternate.UpdatedAt = time.Now()
		alternate.UpdatedBy = "user1"
		err = repo.Update(alternate)

		// Then: 更新が反映される
		require.NoError(t, err)

		found, err := repo.FindByID("PROD001", "PROD002")
		require.NoError(t, err)
		assert.Equal(t, 2, found.Priority)
		assert.Equal(t, "user1", found.UpdatedBy)
	})
}

func TestAlternateProductRepository_Delete(t *testing.T) {
	testDB := testutil.SetupTestDB(t)

	t.Run("代替商品を削除できる", func(t *testing.T) {
		// Given: 商品分類、商品、代替商品が登録されている
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
		products := []*model.Product{
			{
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
			},
			{
				ProductCode:         "PROD002",
				ProductFullName:     "ノートパソコン Let's Note",
				ProductAbbreviation: "LetsNote",
				ProductCategoryCode: "PC001",
				SellingPrice:        180000,
				PurchasePrice:       130000,
				CostOfSales:         130000,
				TaxCategory:         1,
				InventoryManaged:    1,
				CreatedAt:           time.Now(),
				CreatedBy:           "admin",
				UpdatedAt:           time.Now(),
				UpdatedBy:           "admin",
			},
		}
		for _, p := range products {
			err := productRepo.Create(p)
			require.NoError(t, err)
		}

		repo := NewAlternateProductRepository(testDB.DB)
		alternate := &model.AlternateProduct{
			ProductCode:          "PROD001",
			AlternateProductCode: "PROD002",
			Priority:             1,
			CreatedAt:            time.Now(),
			CreatedBy:            "admin",
			UpdatedAt:            time.Now(),
			UpdatedBy:            "admin",
		}
		err = repo.Create(alternate)
		require.NoError(t, err)

		// When: 代替商品を削除すると
		err = repo.Delete("PROD001", "PROD002")

		// Then: 削除される
		require.NoError(t, err)

		found, err := repo.FindByID("PROD001", "PROD002")
		require.NoError(t, err)
		assert.Nil(t, found)
	})
}
