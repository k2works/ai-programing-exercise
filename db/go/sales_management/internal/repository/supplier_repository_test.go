package repository

import (
	"testing"
	"time"

	"github.com/k2works/sales-management-db/internal/model"
	"github.com/k2works/sales-management-db/pkg/testutil"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestSupplierRepository_Create(t *testing.T) {
	testDB := testutil.SetupTestDB(t)

	t.Run("仕入先を登録できる", func(t *testing.T) {
		// Given: 取引先グループと取引先を先に作成
		setupCompanyData(t, testDB)

		// When: 仕入先を登録すると
		repo := NewSupplierRepository(testDB.DB)
		supplier := &model.Supplier{
			SupplierCode:           "COMP001",
			SupplierBranch:         1,
			SupplierCategory:       0,
			SupplierName:           "株式会社ABC 仕入部",
			SupplierNameKana:       "カブシキガイシャエービーシー シイレブ",
			EmployeeCode:           "E001",
			SupplierContactName:    "鈴木一郎",
			SupplierDepartmentName: "営業部",
			SupplierPostalCode:     "100-0001",
			SupplierPrefecture:     "東京都",
			SupplierAddress1:       "千代田区丸の内1-1-1",
			SupplierPhone:          "03-1234-5678",
			SupplierEmail:          "suzuki@abc.co.jp",
			ClosingDay:             31,
			PaymentMonth:           1,
			PaymentDay:             31,
			PaymentMethod:          1,
			CreatedAt:              time.Now(),
			CreatedBy:              "admin",
			UpdatedAt:              time.Now(),
			UpdatedBy:              "admin",
		}
		err := repo.Create(supplier)

		// Then: エラーなく登録できる
		require.NoError(t, err)

		// 登録された仕入先を確認
		found, err := repo.FindByID("COMP001", 1)
		require.NoError(t, err)
		assert.Equal(t, "COMP001", found.SupplierCode)
		assert.Equal(t, 1, found.SupplierBranch)
		assert.Equal(t, "株式会社ABC 仕入部", found.SupplierName)
	})
}

func TestSupplierRepository_FindAll(t *testing.T) {
	testDB := testutil.SetupTestDB(t)

	t.Run("すべての仕入先を取得できる", func(t *testing.T) {
		// Given: 取引先と仕入先が登録されている
		setupCompanyData(t, testDB)

		repo := NewSupplierRepository(testDB.DB)
		suppliers := []*model.Supplier{
			{
				SupplierCode:   "COMP001",
				SupplierBranch: 1,
				SupplierName:   "株式会社ABC 仕入部",
				ClosingDay:     31,
				PaymentMonth:   1,
				PaymentDay:     31,
				PaymentMethod:  1,
				CreatedAt:      time.Now(),
				CreatedBy:      "admin",
				UpdatedAt:      time.Now(),
				UpdatedBy:      "admin",
			},
			{
				SupplierCode:   "COMP001",
				SupplierBranch: 2,
				SupplierName:   "株式会社ABC 物流センター",
				ClosingDay:     31,
				PaymentMonth:   1,
				PaymentDay:     31,
				PaymentMethod:  1,
				CreatedAt:      time.Now(),
				CreatedBy:      "admin",
				UpdatedAt:      time.Now(),
				UpdatedBy:      "admin",
			},
		}

		for _, s := range suppliers {
			err := repo.Create(s)
			require.NoError(t, err)
		}

		// When: すべての仕入先を取得すると
		found, err := repo.FindAll()

		// Then: 登録した仕入先がすべて取得できる
		require.NoError(t, err)
		assert.Len(t, found, 2)
	})
}

func TestSupplierRepository_Update(t *testing.T) {
	testDB := testutil.SetupTestDB(t)

	t.Run("仕入先を更新できる", func(t *testing.T) {
		// Given: 仕入先が登録されている
		setupCompanyData(t, testDB)

		repo := NewSupplierRepository(testDB.DB)
		supplier := &model.Supplier{
			SupplierCode:   "COMP001",
			SupplierBranch: 1,
			SupplierName:   "株式会社ABC 仕入部",
			ClosingDay:     31,
			PaymentMonth:   1,
			PaymentDay:     31,
			PaymentMethod:  1,
			CreatedAt:      time.Now(),
			CreatedBy:      "admin",
			UpdatedAt:      time.Now(),
			UpdatedBy:      "admin",
		}
		err := repo.Create(supplier)
		require.NoError(t, err)

		// When: 仕入先情報を更新すると
		supplier.SupplierName = "株式会社ABC 調達部"
		supplier.UpdatedAt = time.Now()
		supplier.UpdatedBy = "user1"
		err = repo.Update(supplier)

		// Then: 更新が反映される
		require.NoError(t, err)

		found, err := repo.FindByID("COMP001", 1)
		require.NoError(t, err)
		assert.Equal(t, "株式会社ABC 調達部", found.SupplierName)
		assert.Equal(t, "user1", found.UpdatedBy)
	})
}

func TestSupplierRepository_Delete(t *testing.T) {
	testDB := testutil.SetupTestDB(t)

	t.Run("仕入先を削除できる", func(t *testing.T) {
		// Given: 仕入先が登録されている
		setupCompanyData(t, testDB)

		repo := NewSupplierRepository(testDB.DB)
		supplier := &model.Supplier{
			SupplierCode:   "COMP001",
			SupplierBranch: 1,
			SupplierName:   "株式会社ABC 仕入部",
			ClosingDay:     31,
			PaymentMonth:   1,
			PaymentDay:     31,
			PaymentMethod:  1,
			CreatedAt:      time.Now(),
			CreatedBy:      "admin",
			UpdatedAt:      time.Now(),
			UpdatedBy:      "admin",
		}
		err := repo.Create(supplier)
		require.NoError(t, err)

		// When: 仕入先を削除すると
		err = repo.Delete("COMP001", 1)

		// Then: 削除される
		require.NoError(t, err)

		found, err := repo.FindByID("COMP001", 1)
		require.NoError(t, err)
		assert.Nil(t, found)
	})
}
