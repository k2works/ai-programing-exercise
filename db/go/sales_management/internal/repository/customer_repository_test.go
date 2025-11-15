package repository

import (
	"testing"
	"time"

	"github.com/k2works/sales-management-db/internal/model"
	"github.com/k2works/sales-management-db/pkg/testutil"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestCustomerRepository_Create(t *testing.T) {
	testDB := testutil.SetupTestDB(t)

	t.Run("顧客を登録できる", func(t *testing.T) {
		// Given: 取引先グループと取引先を先に作成
		setupCompanyData(t, testDB)

		// When: 顧客を登録すると
		repo := NewCustomerRepository(testDB.DB)
		customer := &model.Customer{
			CustomerCode:           "COMP001",
			CustomerBranch:         1,
			CustomerCategory:       0,
			BillingCode:            "COMP001",
			BillingBranch:          1,
			CollectionCode:         "COMP001",
			CollectionBranch:       1,
			CustomerName:           "株式会社ABC 東京支店",
			CustomerNameKana:       "カブシキガイシャエービーシー トウキョウシテン",
			SalesEmployeeCode:      "E001",
			CustomerContactName:    "山田太郎",
			CustomerDepartmentName: "営業部",
			CustomerPostalCode:     "100-0001",
			CustomerPrefecture:     "東京都",
			CustomerAddress1:       "千代田区丸の内1-1-1",
			CustomerPhone:          "03-1234-5678",
			CustomerEmail:          "yamada@abc.co.jp",
			ClosingDay1:            31,
			PaymentMonth1:          1,
			PaymentDay1:            31,
			PaymentMethod1:         1,
			ClosingDay2:            0,
			CreatedAt:              time.Now(),
			CreatedBy:              "admin",
			UpdatedAt:              time.Now(),
			UpdatedBy:              "admin",
		}
		err := repo.Create(customer)

		// Then: エラーなく登録できる
		require.NoError(t, err)

		// 登録された顧客を確認
		found, err := repo.FindByID("COMP001", 1)
		require.NoError(t, err)
		assert.Equal(t, "COMP001", found.CustomerCode)
		assert.Equal(t, 1, found.CustomerBranch)
		assert.Equal(t, "株式会社ABC 東京支店", found.CustomerName)
	})
}

func TestCustomerRepository_FindAll(t *testing.T) {
	testDB := testutil.SetupTestDB(t)

	t.Run("すべての顧客を取得できる", func(t *testing.T) {
		// Given: 取引先と顧客が登録されている
		setupCompanyData(t, testDB)

		repo := NewCustomerRepository(testDB.DB)
		customers := []*model.Customer{
			{
				CustomerCode:     "COMP001",
				CustomerBranch:   1,
				BillingCode:      "COMP001",
				BillingBranch:    1,
				CollectionCode:   "COMP001",
				CollectionBranch: 1,
				CustomerName:     "株式会社ABC 東京支店",
				ClosingDay1:      31,
				PaymentMonth1:    1,
				PaymentDay1:      31,
				PaymentMethod1:   1,
				CreatedAt:        time.Now(),
				CreatedBy:        "admin",
				UpdatedAt:        time.Now(),
				UpdatedBy:        "admin",
			},
			{
				CustomerCode:     "COMP001",
				CustomerBranch:   2,
				BillingCode:      "COMP001",
				BillingBranch:    1,
				CollectionCode:   "COMP001",
				CollectionBranch: 1,
				CustomerName:     "株式会社ABC 大阪支店",
				ClosingDay1:      31,
				PaymentMonth1:    1,
				PaymentDay1:      31,
				PaymentMethod1:   1,
				CreatedAt:        time.Now(),
				CreatedBy:        "admin",
				UpdatedAt:        time.Now(),
				UpdatedBy:        "admin",
			},
		}

		for _, c := range customers {
			err := repo.Create(c)
			require.NoError(t, err)
		}

		// When: すべての顧客を取得すると
		found, err := repo.FindAll()

		// Then: 登録した顧客がすべて取得できる
		require.NoError(t, err)
		assert.Len(t, found, 2)
	})
}

func TestCustomerRepository_Update(t *testing.T) {
	testDB := testutil.SetupTestDB(t)

	t.Run("顧客を更新できる", func(t *testing.T) {
		// Given: 顧客が登録されている
		setupCompanyData(t, testDB)

		repo := NewCustomerRepository(testDB.DB)
		customer := &model.Customer{
			CustomerCode:     "COMP001",
			CustomerBranch:   1,
			BillingCode:      "COMP001",
			BillingBranch:    1,
			CollectionCode:   "COMP001",
			CollectionBranch: 1,
			CustomerName:     "株式会社ABC 東京支店",
			ClosingDay1:      31,
			PaymentMonth1:    1,
			PaymentDay1:      31,
			PaymentMethod1:   1,
			CreatedAt:        time.Now(),
			CreatedBy:        "admin",
			UpdatedAt:        time.Now(),
			UpdatedBy:        "admin",
		}
		err := repo.Create(customer)
		require.NoError(t, err)

		// When: 顧客情報を更新すると
		customer.CustomerName = "株式会社ABC 東京本店"
		customer.UpdatedAt = time.Now()
		customer.UpdatedBy = "user1"
		err = repo.Update(customer)

		// Then: 更新が反映される
		require.NoError(t, err)

		found, err := repo.FindByID("COMP001", 1)
		require.NoError(t, err)
		assert.Equal(t, "株式会社ABC 東京本店", found.CustomerName)
		assert.Equal(t, "user1", found.UpdatedBy)
	})
}

func TestCustomerRepository_Delete(t *testing.T) {
	testDB := testutil.SetupTestDB(t)

	t.Run("顧客を削除できる", func(t *testing.T) {
		// Given: 顧客が登録されている
		setupCompanyData(t, testDB)

		repo := NewCustomerRepository(testDB.DB)
		customer := &model.Customer{
			CustomerCode:     "COMP001",
			CustomerBranch:   1,
			BillingCode:      "COMP001",
			BillingBranch:    1,
			CollectionCode:   "COMP001",
			CollectionBranch: 1,
			CustomerName:     "株式会社ABC 東京支店",
			ClosingDay1:      31,
			PaymentMonth1:    1,
			PaymentDay1:      31,
			PaymentMethod1:   1,
			CreatedAt:        time.Now(),
			CreatedBy:        "admin",
			UpdatedAt:        time.Now(),
			UpdatedBy:        "admin",
		}
		err := repo.Create(customer)
		require.NoError(t, err)

		// When: 顧客を削除すると
		err = repo.Delete("COMP001", 1)

		// Then: 削除される
		require.NoError(t, err)

		found, err := repo.FindByID("COMP001", 1)
		require.NoError(t, err)
		assert.Nil(t, found)
	})
}

// setupCompanyData はテスト用の取引先グループと取引先を作成します
func setupCompanyData(t *testing.T, testDB *testutil.TestDB) {
	t.Helper()

	// 取引先グループを作成
	groupRepo := NewCompanyGroupRepository(testDB.DB)
	group := &model.CompanyGroup{
		GroupCode: "GRP1",
		GroupName: "優良企業グループ",
		CreatedAt: time.Now(),
		CreatedBy: "admin",
		UpdatedAt: time.Now(),
		UpdatedBy: "admin",
	}
	err := groupRepo.Create(group)
	require.NoError(t, err)

	// 取引先を作成
	companyRepo := NewCompanyRepository(testDB.DB)
	company := &model.Company{
		CompanyCode:         "COMP001",
		CompanyName:         "株式会社ABC",
		CompanyNameKana:     "カブシキガイシャエービーシー",
		SupplierCategory:    0,
		PostalCode:          "100-0001",
		Prefecture:          "東京都",
		Address1:            "千代田区丸の内1-1-1",
		TransactionProhibit: 0,
		CompanyGroupCode:    "GRP1",
		CreditLimit:         10000000,
		CreatedAt:           time.Now(),
		CreatedBy:           "admin",
		UpdatedAt:           time.Now(),
		UpdatedBy:           "admin",
	}
	err = companyRepo.Create(company)
	require.NoError(t, err)
}
