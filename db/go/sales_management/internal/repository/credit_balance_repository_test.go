package repository

import (
	"testing"
	"time"

	"github.com/k2works/sales-management-db/internal/model"
	"github.com/k2works/sales-management-db/pkg/testutil"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestCreditBalanceRepository_Create(t *testing.T) {
	testDB := testutil.SetupTestDB(t)

	setupCreditBalanceTestData(t, testDB)

	t.Run("与信残高を登録できる", func(t *testing.T) {
		// Given
		repo := NewCreditBalanceRepository(testDB.DB)
		balance := &model.CreditBalance{
			CompanyCode:        "COMP001",
			OrderBalance:       1000000,
			AccountsReceivable: 500000,
			AccountsPayable:    200000,
			CreatedAt:          time.Now(),
			CreatedBy:          "admin",
			UpdatedAt:          time.Now(),
			UpdatedBy:          "admin",
		}

		// When
		err := repo.Create(balance)
		require.NoError(t, err)

		// Then
		retrieved, err := repo.FindByCompanyCode("COMP001")
		require.NoError(t, err)
		require.NotNil(t, retrieved)

		assert.Equal(t, "COMP001", retrieved.CompanyCode)
		assert.Equal(t, 1000000, retrieved.OrderBalance)
		assert.Equal(t, 500000, retrieved.AccountsReceivable)
		assert.Equal(t, 200000, retrieved.AccountsPayable)
	})
}

func TestCreditBalanceRepository_Update(t *testing.T) {
	testDB := testutil.SetupTestDB(t)

	setupCreditBalanceTestData(t, testDB)

	t.Run("与信残高を更新できる", func(t *testing.T) {
		// Given
		repo := NewCreditBalanceRepository(testDB.DB)
		balance := &model.CreditBalance{
			CompanyCode:        "COMP001",
			OrderBalance:       1000000,
			AccountsReceivable: 500000,
			AccountsPayable:    200000,
			CreatedAt:          time.Now(),
			CreatedBy:          "admin",
			UpdatedAt:          time.Now(),
			UpdatedBy:          "admin",
		}
		err := repo.Create(balance)
		require.NoError(t, err)

		// When
		balance.OrderBalance = 1500000
		balance.AccountsReceivable = 800000
		balance.UpdatedAt = time.Now()
		err = repo.Update(balance)
		require.NoError(t, err)

		// Then
		updated, err := repo.FindByCompanyCode("COMP001")
		require.NoError(t, err)
		assert.Equal(t, 1500000, updated.OrderBalance)
		assert.Equal(t, 800000, updated.AccountsReceivable)
	})
}

func TestCreditBalanceRepository_Delete(t *testing.T) {
	testDB := testutil.SetupTestDB(t)

	setupCreditBalanceTestData(t, testDB)

	t.Run("与信残高を削除できる", func(t *testing.T) {
		// Given
		repo := NewCreditBalanceRepository(testDB.DB)
		balance := &model.CreditBalance{
			CompanyCode:        "COMP001",
			OrderBalance:       1000000,
			AccountsReceivable: 500000,
			AccountsPayable:    200000,
			CreatedAt:          time.Now(),
			CreatedBy:          "admin",
			UpdatedAt:          time.Now(),
			UpdatedBy:          "admin",
		}
		err := repo.Create(balance)
		require.NoError(t, err)

		// When
		err = repo.Delete("COMP001")
		require.NoError(t, err)

		// Then
		deleted, err := repo.FindByCompanyCode("COMP001")
		require.NoError(t, err)
		assert.Nil(t, deleted)
	})
}

// setupCreditBalanceTestData sets up test data for credit balance tests
func setupCreditBalanceTestData(t *testing.T, testDB *testutil.TestDB) {
	t.Helper()

	// 取引先グループの作成
	companyGroupRepo := NewCompanyGroupRepository(testDB.DB)
	group := &model.CompanyGroup{
		GroupCode: "GRP1",
		GroupName: "テストグループ",
		CreatedAt: time.Now(),
		CreatedBy: "test",
		UpdatedAt: time.Now(),
		UpdatedBy: "test",
	}
	err := companyGroupRepo.Create(group)
	require.NoError(t, err)

	// 取引先の作成
	companyRepo := NewCompanyRepository(testDB.DB)
	company := &model.Company{
		CompanyCode:      "COMP001",
		CompanyGroupCode: "GRP1",
		CompanyName:      "テスト取引先",
		CreatedAt:        time.Now(),
		CreatedBy:        "test",
		UpdatedAt:        time.Now(),
		UpdatedBy:        "test",
	}
	err = companyRepo.Create(company)
	require.NoError(t, err)
}
