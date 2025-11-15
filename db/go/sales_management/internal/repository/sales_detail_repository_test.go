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

// setupSalesDetailTestData はテストデータをセットアップします
func setupSalesDetailTestData(t *testing.T, testDB *testutil.TestDB) {
	t.Helper()

	// 売上リポジトリのテストデータセットアップを再利用
	setupSalesTestData(t, testDB)

	// 売上ヘッダーを作成
	salesRepo := NewSalesRepository(testDB.DB)
	sales := &model.Sales{
		SalesNo:        "SL20240115999",
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
	err := salesRepo.Create(sales)
	require.NoError(t, err)
}

func TestSalesDetailRepository_Create(t *testing.T) {
	testDB := testutil.SetupTestDB(t)

	t.Run("売上明細を登録できる", func(t *testing.T) {
		repo := NewSalesDetailRepository(testDB.DB)
		setupSalesDetailTestData(t, testDB)

		detail := &model.SalesDetail{
			SalesNo:          "SL20240115999",
			DetailNo:         1,
			ProductCode:      "PROD001",
			ProductName:      sql.NullString{String: "商品A", Valid: true},
			SellingPrice:     1000,
			ShippedQuantity:  10,
			Quantity:         10,
			DiscountAmount:   0,
			BillingDate:      sql.NullTime{Valid: false},
			InvoiceNo:        sql.NullString{Valid: false},
			BillingDelayFlag: sql.NullInt32{Valid: false},
			AutoJournalDate:  sql.NullTime{Valid: false},
			CreatedAt:        time.Now(),
			CreatedBy:        "admin",
			UpdatedAt:        time.Now(),
			UpdatedBy:        "admin",
		}

		err := repo.Create(detail)
		require.NoError(t, err)

		// 取得して確認
		details, err := repo.FindBySalesNo("SL20240115999")
		require.NoError(t, err)
		assert.Len(t, details, 1)
		assert.Equal(t, 10, details[0].Quantity)
		assert.Equal(t, "商品A", details[0].ProductName.String)
	})
}

func TestSalesDetailRepository_FindBySalesNo(t *testing.T) {
	testDB := testutil.SetupTestDB(t)

	t.Run("売上番号で明細を取得できる", func(t *testing.T) {
		repo := NewSalesDetailRepository(testDB.DB)
		setupSalesDetailTestData(t, testDB)

		// 複数明細を作成
		details := []*model.SalesDetail{
			{
				SalesNo:         "SL20240115999",
				DetailNo:        1,
				ProductCode:     "PROD001",
				ProductName:     sql.NullString{String: "商品A", Valid: true},
				SellingPrice:    1000,
				ShippedQuantity: 10,
				Quantity:        10,
				CreatedAt:       time.Now(),
				CreatedBy:       "admin",
				UpdatedAt:       time.Now(),
				UpdatedBy:       "admin",
			},
			{
				SalesNo:         "SL20240115999",
				DetailNo:        2,
				ProductCode:     "PROD001",
				ProductName:     sql.NullString{String: "商品A", Valid: true},
				SellingPrice:    1000,
				ShippedQuantity: 5,
				Quantity:        5,
				CreatedAt:       time.Now(),
				CreatedBy:       "admin",
				UpdatedAt:       time.Now(),
				UpdatedBy:       "admin",
			},
		}

		for _, detail := range details {
			err := repo.Create(detail)
			require.NoError(t, err)
		}

		found, err := repo.FindBySalesNo("SL20240115999")
		require.NoError(t, err)
		assert.Equal(t, 2, len(found))
		assert.Equal(t, 1, found[0].DetailNo)
		assert.Equal(t, 2, found[1].DetailNo)
	})
}

func TestSalesDetailRepository_Update(t *testing.T) {
	testDB := testutil.SetupTestDB(t)

	t.Run("売上明細を更新できる", func(t *testing.T) {
		repo := NewSalesDetailRepository(testDB.DB)
		setupSalesDetailTestData(t, testDB)

		detail := &model.SalesDetail{
			SalesNo:         "SL20240115999",
			DetailNo:        1,
			ProductCode:     "PROD001",
			ProductName:     sql.NullString{String: "商品A", Valid: true},
			SellingPrice:    1000,
			ShippedQuantity: 10,
			Quantity:        10,
			DiscountAmount:  0,
			CreatedAt:       time.Now(),
			CreatedBy:       "admin",
			UpdatedAt:       time.Now(),
			UpdatedBy:       "admin",
		}
		err := repo.Create(detail)
		require.NoError(t, err)

		// 更新
		detail.Quantity = 15
		detail.DiscountAmount = 500
		detail.UpdatedAt = time.Now()
		detail.UpdatedBy = "admin2"

		err = repo.Update(detail)
		require.NoError(t, err)

		// 確認
		found, err := repo.FindByID("SL20240115999", 1)
		require.NoError(t, err)
		assert.Equal(t, 15, found.Quantity)
		assert.Equal(t, 500, found.DiscountAmount)
		assert.Equal(t, "admin2", found.UpdatedBy)
	})
}

func TestSalesDetailRepository_Delete(t *testing.T) {
	testDB := testutil.SetupTestDB(t)

	t.Run("売上明細を削除できる", func(t *testing.T) {
		repo := NewSalesDetailRepository(testDB.DB)
		setupSalesDetailTestData(t, testDB)

		detail := &model.SalesDetail{
			SalesNo:         "SL20240115999",
			DetailNo:        1,
			ProductCode:     "PROD001",
			ProductName:     sql.NullString{String: "商品A", Valid: true},
			SellingPrice:    1000,
			ShippedQuantity: 10,
			Quantity:        10,
			CreatedAt:       time.Now(),
			CreatedBy:       "admin",
			UpdatedAt:       time.Now(),
			UpdatedBy:       "admin",
		}
		err := repo.Create(detail)
		require.NoError(t, err)

		// 削除
		err = repo.Delete("SL20240115999", 1)
		require.NoError(t, err)

		// 削除を確認
		found, err := repo.FindByID("SL20240115999", 1)
		require.NoError(t, err)
		assert.Nil(t, found)
	})
}
