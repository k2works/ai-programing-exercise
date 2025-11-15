package repository

import (
	"testing"
	"time"

	"github.com/k2works/sales-management-db/internal/model"
	"github.com/k2works/sales-management-db/pkg/testutil"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestSalesOrderRepository_Create(t *testing.T) {
	testDB := testutil.SetupTestDB(t)

	t.Run("受注を登録できる", func(t *testing.T) {
		// Given: テストデータの準備（顧客、社員、部門）
		setupOrderTestData(t, testDB)

		repo := NewSalesOrderRepository(testDB.DB)
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

		// When: 受注を登録すると
		err := repo.Create(order)

		// Then: エラーなく登録できる
		require.NoError(t, err)

		// 登録された受注を確認
		found, err := repo.FindByID("SO20250108001")
		require.NoError(t, err)
		assert.Equal(t, "SO20250108001", found.OrderNo)
		assert.Equal(t, "COMP001", found.CustomerCode)
		assert.Equal(t, 50000, found.OrderAmount)
	})
}

func setupOrderTestData(t *testing.T, testDB *testutil.TestDB) {
	t.Helper()

	// 部門の作成
	deptRepo := NewDepartmentRepository(testDB.DB)
	dept := &model.Department{
		DepartmentCode: "11101",
		StartDate:      time.Date(2021, 1, 1, 0, 0, 0, 0, time.UTC),
		EndDate:        time.Date(2100, 12, 31, 0, 0, 0, 0, time.UTC),
		DepartmentName: "営業部",
		HierarchyLevel: 1,
		DepartmentPath: "/11101/",
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
		EmployeeCode:     "EMP001",
		EmployeeName:     "山田太郎",
		EmployeeNameKana: "ヤマダタロウ",
		Password:         "password",
		DepartmentCode:   "11101",
		StartDate:        time.Date(2021, 1, 1, 0, 0, 0, 0, time.UTC),
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
		CompanyCode:      "COMP001",
		CompanyName:      "株式会社テスト",
		CompanyNameKana:  "カブシキガイシャテスト",
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
		CustomerCode:     "COMP001",
		CustomerBranch:   1,
		BillingCode:      "COMP001",
		BillingBranch:    1,
		CollectionCode:   "COMP001",
		CollectionBranch: 1,
		SalesEmployeeCode: "EMP001",
		CreatedAt:        time.Now(),
		CreatedBy:        "admin",
		UpdatedAt:        time.Now(),
		UpdatedBy:        "admin",
	}
	err = custRepo.Create(customer)
	require.NoError(t, err)
}
