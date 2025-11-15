package repository

import (
	"testing"
	"time"

	"github.com/k2works/sales-management-db/internal/model"
	"github.com/k2works/sales-management-db/pkg/testutil"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestOrderDetailRepository_Create(t *testing.T) {
	testDB := testutil.SetupTestDB(t)

	t.Run("受注明細を登録できる", func(t *testing.T) {
		// Given: テストデータの準備（受注と商品）
		setupOrderDetailTestData(t, testDB)

		repo := NewOrderDetailRepository(testDB.DB)
		detail := &model.OrderDetail{
			OrderNo:              "SO20250108001",
			DetailNo:             1,
			ProductCode:          "PROD001",
			ProductName:          "商品A",
			SalesUnitPrice:       1000,
			Quantity:             10,
			ConsumptionTaxRate:   10,
			AllocatedQuantity:    0,
			ShippingOrderQuantity: 0,
			ShippedQuantity:      0,
			CompletionFlag:       0,
			DiscountAmount:       0,
			DeliveryDate:         time.Date(2025, 1, 15, 0, 0, 0, 0, time.UTC),
			CreatedAt:            time.Now(),
			CreatedBy:            "admin",
			UpdatedAt:            time.Now(),
			UpdatedBy:            "admin",
		}

		// When: 受注明細を登録すると
		err := repo.Create(detail)

		// Then: エラーなく登録できる
		require.NoError(t, err)

		// 登録された受注明細を確認
		found, err := repo.FindByID("SO20250108001", 1)
		require.NoError(t, err)
		assert.Equal(t, "SO20250108001", found.OrderNo)
		assert.Equal(t, 1, found.DetailNo)
		assert.Equal(t, "PROD001", found.ProductCode)
		assert.Equal(t, 10, found.Quantity)
	})
}

func TestOrderDetailRepository_FindByOrderNo(t *testing.T) {
	testDB := testutil.SetupTestDB(t)

	t.Run("受注番号で明細を取得できる", func(t *testing.T) {
		// Given: 複数の明細が登録されている
		setupOrderDetailTestData(t, testDB)

		repo := NewOrderDetailRepository(testDB.DB)
		details := []*model.OrderDetail{
			{
				OrderNo:        "SO20250108001",
				DetailNo:       1,
				ProductCode:    "PROD001",
				ProductName:    "商品A",
				SalesUnitPrice: 1000,
				Quantity:       10,
				CreatedAt:      time.Now(),
				CreatedBy:      "admin",
				UpdatedAt:      time.Now(),
				UpdatedBy:      "admin",
			},
			{
				OrderNo:        "SO20250108001",
				DetailNo:       2,
				ProductCode:    "PROD002",
				ProductName:    "商品B",
				SalesUnitPrice: 2000,
				Quantity:       5,
				CreatedAt:      time.Now(),
				CreatedBy:      "admin",
				UpdatedAt:      time.Now(),
				UpdatedBy:      "admin",
			},
		}

		for _, d := range details {
			err := repo.Create(d)
			require.NoError(t, err)
		}

		// When: 受注番号で明細を取得すると
		found, err := repo.FindByOrderNo("SO20250108001")

		// Then: 登録した明細がすべて取得できる
		require.NoError(t, err)
		assert.Len(t, found, 2)
		assert.Equal(t, 1, found[0].DetailNo)
		assert.Equal(t, 2, found[1].DetailNo)
	})
}

func TestOrderDetailRepository_Update(t *testing.T) {
	testDB := testutil.SetupTestDB(t)

	t.Run("受注明細を更新できる", func(t *testing.T) {
		// Given: 受注明細が登録されている
		setupOrderDetailTestData(t, testDB)

		repo := NewOrderDetailRepository(testDB.DB)
		detail := &model.OrderDetail{
			OrderNo:        "SO20250108001",
			DetailNo:       1,
			ProductCode:    "PROD001",
			ProductName:    "商品A",
			SalesUnitPrice: 1000,
			Quantity:       10,
			CreatedAt:      time.Now(),
			CreatedBy:      "admin",
			UpdatedAt:      time.Now(),
			UpdatedBy:      "admin",
		}
		err := repo.Create(detail)
		require.NoError(t, err)

		// When: 出荷数量を更新すると
		detail.ShippedQuantity = 10
		detail.CompletionFlag = 1
		detail.UpdatedAt = time.Now()
		detail.UpdatedBy = "user1"
		err = repo.Update(detail)

		// Then: 更新が反映される
		require.NoError(t, err)

		found, err := repo.FindByID("SO20250108001", 1)
		require.NoError(t, err)
		assert.Equal(t, 10, found.ShippedQuantity)
		assert.Equal(t, 1, found.CompletionFlag)
		assert.Equal(t, "user1", found.UpdatedBy)
	})
}

func TestOrderDetailRepository_Delete(t *testing.T) {
	testDB := testutil.SetupTestDB(t)

	t.Run("受注明細を削除できる", func(t *testing.T) {
		// Given: 受注明細が登録されている
		setupOrderDetailTestData(t, testDB)

		repo := NewOrderDetailRepository(testDB.DB)
		detail := &model.OrderDetail{
			OrderNo:        "SO20250108001",
			DetailNo:       1,
			ProductCode:    "PROD001",
			ProductName:    "商品A",
			SalesUnitPrice: 1000,
			Quantity:       10,
			CreatedAt:      time.Now(),
			CreatedBy:      "admin",
			UpdatedAt:      time.Now(),
			UpdatedBy:      "admin",
		}
		err := repo.Create(detail)
		require.NoError(t, err)

		// When: 受注明細を削除すると
		err = repo.Delete("SO20250108001", 1)

		// Then: 削除される
		require.NoError(t, err)

		found, err := repo.FindByID("SO20250108001", 1)
		require.NoError(t, err)
		assert.Nil(t, found)
	})
}

func setupOrderDetailTestData(t *testing.T, testDB *testutil.TestDB) {
	t.Helper()

	// 受注データのセットアップ（部門、社員、取引先グループ、取引先、顧客）
	setupOrderTestData(t, testDB)

	// 商品分類の作成
	categoryRepo := NewProductCategoryRepository(testDB.DB)
	category := &model.ProductCategory{
		CategoryCode:  "CAT01",
		CategoryName:  "一般商品",
		CategoryPath:  "/CAT01/",
		CategoryLevel: 1,
		IsLowestLevel: 1,
		CreatedAt:     time.Now(),
		CreatedBy:     "admin",
		UpdatedAt:     time.Now(),
		UpdatedBy:     "admin",
	}
	err := categoryRepo.Create(category)
	require.NoError(t, err)

	// 商品の作成
	productRepo := NewProductRepository(testDB.DB)
	products := []*model.Product{
		{
			ProductCode:         "PROD001",
			ProductFullName:     "商品A",
			ProductAbbreviation: "商品A",
			ProductNameKana:     "ショウヒンエー",
			ProductCategoryCode: "CAT01",
			SellingPrice:        1000,
			PurchasePrice:       500,
			CostOfSales:         500,
			TaxCategory:         1,
			MiscCategory:        0,
			InventoryManaged:    1,
			StockAllocation:     1,
			SupplierCode:        "",
			SupplierBranch:      0,
			CreatedAt:           time.Now(),
			CreatedBy:           "admin",
			UpdatedAt:           time.Now(),
			UpdatedBy:           "admin",
		},
		{
			ProductCode:         "PROD002",
			ProductFullName:     "商品B",
			ProductAbbreviation: "商品B",
			ProductNameKana:     "ショウヒンビー",
			ProductCategoryCode: "CAT01",
			SellingPrice:        2000,
			PurchasePrice:       1000,
			CostOfSales:         1000,
			TaxCategory:         1,
			MiscCategory:        0,
			InventoryManaged:    1,
			StockAllocation:     1,
			SupplierCode:        "",
			SupplierBranch:      0,
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

	// 受注の作成
	orderRepo := NewSalesOrderRepository(testDB.DB)
	order := &model.SalesOrder{
		OrderNo:         "SO20250108001",
		OrderDate:       time.Date(2025, 1, 8, 10, 0, 0, 0, time.UTC),
		DepartmentCode:  "11101",
		StartDate:       time.Date(2021, 1, 1, 0, 0, 0, 0, time.UTC),
		CustomerCode:    "COMP001",
		CustomerBranch:  1,
		EmployeeCode:    "EMP001",
		DeliveryDate:    time.Date(2025, 1, 15, 0, 0, 0, 0, time.UTC),
		CustomerOrderNo: "CUST-ORDER-001",
		OrderAmount:     50000,
		ConsumptionTax:  5000,
		DocumentComment: "初回注文",
		CreatedAt:       time.Now(),
		CreatedBy:       "admin",
		UpdatedAt:       time.Now(),
		UpdatedBy:       "admin",
	}
	err = orderRepo.Create(order)
	require.NoError(t, err)
}
