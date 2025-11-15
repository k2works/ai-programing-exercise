package repository

import (
	"database/sql"
	"testing"
	"time"

	"github.com/k2works/sales-management-db/internal/model"
	"github.com/k2works/sales-management-db/pkg/testutil"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// setupSalesTestData はテストデータをセットアップします
func setupSalesTestData(t *testing.T, testDB *testutil.TestDB) {
	t.Helper()

	// 部門の作成
	deptRepo := NewDepartmentRepository(testDB.DB)
	dept := &model.Department{
		DepartmentCode: "120000",
		StartDate:      time.Date(2024, 1, 1, 0, 0, 0, 0, time.UTC),
		EndDate:        time.Date(2100, 12, 31, 0, 0, 0, 0, time.UTC),
		DepartmentName: "営業部",
		HierarchyLevel: 1,
		DepartmentPath: "/120000/",
		IsLowestLevel:  1,
		CreatedAt:      time.Now(),
		CreatedBy:      "admin",
		UpdatedAt:      time.Now(),
		UpdatedBy:      "admin",
	}
	err := deptRepo.Create(dept)
	require.NoError(t, err)

	// 社員の作成
	empRepo := NewEmployeeRepository(testDB.DB)
	emp := &model.Employee{
		EmployeeCode:     "E00001",
		EmployeeName:     "山田太郎",
		EmployeeNameKana: "ヤマダタロウ",
		Password:         "password",
		DepartmentCode:   "120000",
		StartDate:        time.Date(2024, 1, 1, 0, 0, 0, 0, time.UTC),
		CreatedAt:        time.Now(),
		CreatedBy:        "admin",
		UpdatedAt:        time.Now(),
		UpdatedBy:        "admin",
	}
	err = empRepo.Create(emp)
	require.NoError(t, err)

	// 取引先グループの作成
	groupRepo := NewCompanyGroupRepository(testDB.DB)
	group := &model.CompanyGroup{
		GroupCode: "GRP1",
		GroupName: "優良企業",
		CreatedAt: time.Now(),
		CreatedBy: "admin",
		UpdatedAt: time.Now(),
		UpdatedBy: "admin",
	}
	err = groupRepo.Create(group)
	require.NoError(t, err)

	// 取引先の作成
	compRepo := NewCompanyRepository(testDB.DB)
	company := &model.Company{
		CompanyCode:      "CUST001",
		CompanyName:      "株式会社サンプル顧客",
		CompanyNameKana:  "カブシキガイシャサンプルコキャク",
		CompanyGroupCode: "GRP1",
		CreatedAt:        time.Now(),
		CreatedBy:        "admin",
		UpdatedAt:        time.Now(),
		UpdatedBy:        "admin",
	}
	err = compRepo.Create(company)
	require.NoError(t, err)

	// 顧客の作成
	custRepo := NewCustomerRepository(testDB.DB)
	customer := &model.Customer{
		CustomerCode:      "CUST001",
		CustomerBranch:    0,
		BillingCode:       "CUST001",
		BillingBranch:     0,
		CollectionCode:    "CUST001",
		CollectionBranch:  0,
		SalesEmployeeCode: "E00001",
		CreatedAt:         time.Now(),
		CreatedBy:         "admin",
		UpdatedAt:         time.Now(),
		UpdatedBy:         "admin",
	}
	err = custRepo.Create(customer)
	require.NoError(t, err)

	// 受注の作成
	orderRepo := NewSalesOrderRepository(testDB.DB)
	order := &model.SalesOrder{
		OrderNo:        "SO20240110001",
		OrderDate:      time.Now(),
		CustomerCode:   "CUST001",
		CustomerBranch: 0,
		DepartmentCode: "120000",
		StartDate:      time.Date(2024, 1, 1, 0, 0, 0, 0, time.UTC),
		EmployeeCode:   "E00001",
		OrderAmount:    100000,
		ConsumptionTax: 10000,
		CreatedAt:      time.Now(),
		CreatedBy:      "admin",
		UpdatedAt:      time.Now(),
		UpdatedBy:      "admin",
	}
	err = orderRepo.Create(order)
	require.NoError(t, err)

	// 商品分類作成
	productCategoryRepo := NewProductCategoryRepository(testDB.DB)
	category := &model.ProductCategory{
		CategoryCode:  "CAT001",
		CategoryName:  "一般商品",
		CategoryLevel: 1,
		CategoryPath:  "/CAT001/",
		IsLowestLevel: 1,
		CreatedAt:     time.Now(),
		CreatedBy:     "admin",
		UpdatedAt:     time.Now(),
		UpdatedBy:     "admin",
	}
	err = productCategoryRepo.Create(category)
	require.NoError(t, err)

	// 商品作成
	productRepo := NewProductRepository(testDB.DB)
	product := &model.Product{
		ProductCode:         "PROD001",
		ProductFullName:     "商品A",
		ProductCategoryCode: "CAT001",
		SellingPrice:        1000,
		PurchasePrice:       800,
		CostOfSales:         750,
		TaxCategory:         1,
		InventoryManaged:    1,
		StockAllocation:     1,
		CreatedAt:           time.Now(),
		CreatedBy:           "admin",
		UpdatedAt:           time.Now(),
		UpdatedBy:           "admin",
	}
	err = productRepo.Create(product)
	require.NoError(t, err)
}

func TestSalesRepository_Create(t *testing.T) {
	testDB := testutil.SetupTestDB(t)

	t.Run("売上を登録できる", func(t *testing.T) {
		repo := NewSalesRepository(testDB.DB)
		setupSalesTestData(t, testDB)

		sales := &model.Sales{
			SalesNo:        "SL20240115001",
			SalesDate:      sql.NullTime{Time: time.Now(), Valid: true},
			SalesCategory:  1,
			OrderNo:        sql.NullString{String: "SO20240110001", Valid: true},
			DepartmentCode: "120000",
			StartDate:      time.Date(2024, 1, 1, 0, 0, 0, 0, time.UTC),
			CompanyCode:    "CUST001",
			SalesAmount:    100000,
			ConsumptionTax: 10000,
			CorrectionNo:   sql.NullString{Valid: false},
			OriginalSlipNo: sql.NullString{Valid: false},
			CreatedAt:      time.Now(),
			CreatedBy:      "admin",
			UpdatedAt:      time.Now(),
			UpdatedBy:      "admin",
		}

		err := repo.Create(sales)
		require.NoError(t, err)

		// 登録された売上を確認
		found, err := repo.FindByID("SL20240115001")
		require.NoError(t, err)
		assert.Equal(t, "SO20240110001", found.OrderNo.String)
		assert.Equal(t, 100000, found.SalesAmount)
	})
}

func TestSalesRepository_FindByOrderNo(t *testing.T) {
	testDB := testutil.SetupTestDB(t)

	t.Run("受注番号で売上を検索できる", func(t *testing.T) {
		repo := NewSalesRepository(testDB.DB)
		setupSalesTestData(t, testDB)

		// 売上作成
		sales := &model.Sales{
			SalesNo:        "SL20240115002",
			SalesDate:      sql.NullTime{Time: time.Now(), Valid: true},
			SalesCategory:  1,
			OrderNo:        sql.NullString{String: "SO20240110001", Valid: true},
			DepartmentCode: "120000",
			StartDate:      time.Date(2024, 1, 1, 0, 0, 0, 0, time.UTC),
			CompanyCode:    "CUST001",
			SalesAmount:    50000,
			ConsumptionTax: 5000,
			CreatedAt:      time.Now(),
			CreatedBy:      "admin",
			UpdatedAt:      time.Now(),
			UpdatedBy:      "admin",
		}

		err := repo.Create(sales)
		require.NoError(t, err)

		salesList, err := repo.FindByOrderNo("SO20240110001")
		require.NoError(t, err)
		assert.GreaterOrEqual(t, len(salesList), 1)
	})
}

func TestSalesRepository_Update(t *testing.T) {
	testDB := testutil.SetupTestDB(t)

	t.Run("売上を更新できる", func(t *testing.T) {
		repo := NewSalesRepository(testDB.DB)
		setupSalesTestData(t, testDB)

		// 作成
		sales := &model.Sales{
			SalesNo:        "SL20240115003",
			SalesDate:      sql.NullTime{Time: time.Now(), Valid: true},
			SalesCategory:  1,
			DepartmentCode: "120000",
			StartDate:      time.Date(2024, 1, 1, 0, 0, 0, 0, time.UTC),
			CompanyCode:    "CUST001",
			SalesAmount:    100000,
			ConsumptionTax: 10000,
			CreatedAt:      time.Now(),
			CreatedBy:      "admin",
			UpdatedAt:      time.Now(),
			UpdatedBy:      "admin",
		}
		err := repo.Create(sales)
		require.NoError(t, err)

		// 更新
		sales.SalesAmount = 120000
		sales.ConsumptionTax = 12000
		sales.UpdatedAt = time.Now()

		err = repo.Update(sales)
		require.NoError(t, err)

		// 確認
		updated, err := repo.FindByID("SL20240115003")
		require.NoError(t, err)
		assert.Equal(t, 120000, updated.SalesAmount)
		assert.Equal(t, 12000, updated.ConsumptionTax)
	})
}

func TestSalesRepository_RedBlackSlips(t *testing.T) {
	testDB := testutil.SetupTestDB(t)

	t.Run("赤黒伝票による訂正ができる", func(t *testing.T) {
		repo := NewSalesRepository(testDB.DB)
		setupSalesTestData(t, testDB)

		// 元伝票作成
		original := &model.Sales{
			SalesNo:        "SL20240115004",
			SalesDate:      sql.NullTime{Time: time.Now(), Valid: true},
			SalesCategory:  1,
			DepartmentCode: "120000",
			StartDate:      time.Date(2024, 1, 1, 0, 0, 0, 0, time.UTC),
			CompanyCode:    "CUST001",
			SalesAmount:    100000,
			ConsumptionTax: 10000,
			CreatedAt:      time.Now(),
			CreatedBy:      "admin",
			UpdatedAt:      time.Now(),
			UpdatedBy:      "admin",
		}
		err := repo.Create(original)
		require.NoError(t, err)

		// 赤伝票（マイナス）
		redSlip := &model.Sales{
			SalesNo:        "SL20240115005",
			SalesDate:      sql.NullTime{Time: time.Now(), Valid: true},
			SalesCategory:  2, // 返品
			DepartmentCode: "120000",
			StartDate:      time.Date(2024, 1, 1, 0, 0, 0, 0, time.UTC),
			CompanyCode:    "CUST001",
			SalesAmount:    -100000,
			ConsumptionTax: -10000,
			OriginalSlipNo: sql.NullString{String: "SL20240115004", Valid: true},
			CreatedAt:      time.Now(),
			CreatedBy:      "admin",
			UpdatedAt:      time.Now(),
			UpdatedBy:      "admin",
		}
		err = repo.Create(redSlip)
		require.NoError(t, err)

		// 黒伝票（プラス、訂正後）
		blackSlip := &model.Sales{
			SalesNo:        "SL20240115006",
			SalesDate:      sql.NullTime{Time: time.Now(), Valid: true},
			SalesCategory:  1,
			DepartmentCode: "120000",
			StartDate:      time.Date(2024, 1, 1, 0, 0, 0, 0, time.UTC),
			CompanyCode:    "CUST001",
			SalesAmount:    120000,
			ConsumptionTax: 12000,
			CorrectionNo:   sql.NullString{String: "SL20240115005", Valid: true},
			CreatedAt:      time.Now(),
			CreatedBy:      "admin",
			UpdatedAt:      time.Now(),
			UpdatedBy:      "admin",
		}
		err = repo.Create(blackSlip)
		require.NoError(t, err)

		// 確認
		retrieved, err := repo.FindByID("SL20240115005")
		require.NoError(t, err)
		assert.Equal(t, "SL20240115004", retrieved.OriginalSlipNo.String)
		assert.Equal(t, -100000, retrieved.SalesAmount)
	})
}

func TestSalesRepository_Delete(t *testing.T) {
	testDB := testutil.SetupTestDB(t)

	t.Run("売上を削除できる", func(t *testing.T) {
		repo := NewSalesRepository(testDB.DB)
		setupSalesTestData(t, testDB)

		sales := &model.Sales{
			SalesNo:        "SL20240115007",
			SalesDate:      sql.NullTime{Time: time.Now(), Valid: true},
			SalesCategory:  1,
			DepartmentCode: "120000",
			StartDate:      time.Date(2024, 1, 1, 0, 0, 0, 0, time.UTC),
			CompanyCode:    "CUST001",
			SalesAmount:    100000,
			ConsumptionTax: 10000,
			CreatedAt:      time.Now(),
			CreatedBy:      "admin",
			UpdatedAt:      time.Now(),
			UpdatedBy:      "admin",
		}
		err := repo.Create(sales)
		require.NoError(t, err)

		// 削除
		err = repo.Delete("SL20240115007")
		require.NoError(t, err)

		// 削除を確認
		found, err := repo.FindByID("SL20240115007")
		require.NoError(t, err)
		assert.Nil(t, found)
	})
}
