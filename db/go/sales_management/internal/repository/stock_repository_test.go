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

// setupStockTestData はテストデータをセットアップします
func setupStockTestData(t *testing.T, repo *StockRepository) {
	t.Helper()

	// 倉庫データを作成
	warehouseRepo := NewWarehouseRepository(repo.db)
	warehouses := []*model.Warehouse{
		{
			WarehouseCode: "WH1",
			WarehouseName: "東京倉庫",
			CreatedAt:     time.Now(),
			CreatedBy:     "admin",
			UpdatedAt:     time.Now(),
			UpdatedBy:     "admin",
		},
		{
			WarehouseCode: "WH2",
			WarehouseName: "大阪倉庫",
			CreatedAt:     time.Now(),
			CreatedBy:     "admin",
			UpdatedAt:     time.Now(),
			UpdatedBy:     "admin",
		},
	}
	for _, wh := range warehouses {
		err := warehouseRepo.Create(wh)
		require.NoError(t, err)
	}

	// 商品分類データを作成
	categoryRepo := NewProductCategoryRepository(repo.db)
	categories := []*model.ProductCategory{
		{
			CategoryCode:  "01",
			CategoryName:  "電子機器",
			CategoryLevel: 1,
			CategoryPath:  "01",
			IsLowestLevel: 1,
			CreatedAt:     time.Now(),
			CreatedBy:     "admin",
			UpdatedAt:     time.Now(),
			UpdatedBy:     "admin",
		},
	}
	for _, cat := range categories {
		err := categoryRepo.Create(cat)
		require.NoError(t, err)
	}

	// 商品データを作成
	productRepo := NewProductRepository(repo.db)
	products := []*model.Product{
		{
			ProductCode:         "PRD001",
			ProductFullName:     "ノートPC",
			ProductCategoryCode: "01",
			SellingPrice:        100000,
			PurchasePrice:       80000,
			CostOfSales:         75000,
			TaxCategory:         1,
			InventoryManaged:    1,
			StockAllocation:     1,
			CreatedAt:           time.Now(),
			CreatedBy:           "admin",
			UpdatedAt:           time.Now(),
			UpdatedBy:           "admin",
		},
		{
			ProductCode:         "PRD002",
			ProductFullName:     "デスクトップPC",
			ProductCategoryCode: "01",
			SellingPrice:        150000,
			PurchasePrice:       120000,
			CostOfSales:         110000,
			TaxCategory:         1,
			InventoryManaged:    1,
			StockAllocation:     1,
			CreatedAt:           time.Now(),
			CreatedBy:           "admin",
			UpdatedAt:           time.Now(),
			UpdatedBy:           "admin",
		},
	}
	for _, prd := range products {
		err := productRepo.Create(prd)
		require.NoError(t, err)
	}
}

func TestStockRepository_Create(t *testing.T) {
	testDB := testutil.SetupTestDB(t)

	t.Run("在庫を登録できる", func(t *testing.T) {
		repo := NewStockRepository(testDB.DB)
		setupStockTestData(t, repo)

		stock := &model.Stock{
			WarehouseCode:    "WH1",
			ProductCode:      "PRD001",
			LotNumber:        "LOT20250108001",
			StockCategory:    "1", // 通常在庫
			QualityCategory:  "A", // 良品
			ActualStock:      100,
			AvailableStock:   90,
			LastShippingDate: sql.NullTime{Valid: false},
			CreatedAt:        time.Now(),
			CreatedBy:        "admin",
			UpdatedAt:        time.Now(),
			UpdatedBy:        "admin",
		}

		err := repo.Create(stock)
		require.NoError(t, err)

		// 登録された在庫を確認
		found, err := repo.FindByID("WH1", "PRD001", "LOT20250108001", "1", "A")
		require.NoError(t, err)
		assert.Equal(t, "WH1", found.WarehouseCode)
		assert.Equal(t, "PRD001", found.ProductCode)
		assert.Equal(t, "LOT20250108001", found.LotNumber)
		assert.Equal(t, "1", found.StockCategory)
		assert.Equal(t, "A", found.QualityCategory)
		assert.Equal(t, 100, found.ActualStock)
		assert.Equal(t, 90, found.AvailableStock)
	})
}

func TestStockRepository_FindByID(t *testing.T) {
	t.Run("複合主キーで在庫を取得できる", func(t *testing.T) {
		testDB := testutil.SetupTestDB(t)
		repo := NewStockRepository(testDB.DB)
		setupStockTestData(t, repo)

		// テストデータを作成
		stock := &model.Stock{
			WarehouseCode:    "WH1",
			ProductCode:      "PRD001",
			LotNumber:        "LOT20250108001",
			StockCategory:    "1",
			QualityCategory:  "A",
			ActualStock:      100,
			AvailableStock:   90,
			LastShippingDate: sql.NullTime{Valid: false},
			CreatedAt:        time.Now(),
			CreatedBy:        "admin",
			UpdatedAt:        time.Now(),
			UpdatedBy:        "admin",
		}
		err := repo.Create(stock)
		require.NoError(t, err)

		// 取得
		found, err := repo.FindByID("WH1", "PRD001", "LOT20250108001", "1", "A")
		require.NoError(t, err)
		assert.NotNil(t, found)
		assert.Equal(t, "WH1", found.WarehouseCode)
	})

	t.Run("存在しない在庫はnilを返す", func(t *testing.T) {
		testDB := testutil.SetupTestDB(t)
		repo := NewStockRepository(testDB.DB)
		setupStockTestData(t, repo)

		found, err := repo.FindByID("WH1", "PRD001", "NOTFOUND", "1", "A")
		require.NoError(t, err)
		assert.Nil(t, found)
	})
}

func TestStockRepository_FindAll(t *testing.T) {
	t.Run("全ての在庫を取得できる", func(t *testing.T) {
		testDB := testutil.SetupTestDB(t)
		repo := NewStockRepository(testDB.DB)
		setupStockTestData(t, repo)

		stocks := []*model.Stock{
			{
				WarehouseCode:    "WH1",
				ProductCode:      "PRD001",
				LotNumber:        "LOT001",
				StockCategory:    "1",
				QualityCategory:  "A",
				ActualStock:      100,
				AvailableStock:   90,
				LastShippingDate: sql.NullTime{Valid: false},
				CreatedAt:        time.Now(),
				CreatedBy:        "admin",
				UpdatedAt:        time.Now(),
				UpdatedBy:        "admin",
			},
			{
				WarehouseCode:    "WH2",
				ProductCode:      "PRD002",
				LotNumber:        "LOT002",
				StockCategory:    "1",
				QualityCategory:  "A",
				ActualStock:      200,
				AvailableStock:   180,
				LastShippingDate: sql.NullTime{Valid: false},
				CreatedAt:        time.Now(),
				CreatedBy:        "admin",
				UpdatedAt:        time.Now(),
				UpdatedBy:        "admin",
			},
		}

		for _, stock := range stocks {
			err := repo.Create(stock)
			require.NoError(t, err)
		}

		found, err := repo.FindAll()
		require.NoError(t, err)
		assert.GreaterOrEqual(t, len(found), 2)
	})
}

func TestStockRepository_FindByWarehouse(t *testing.T) {
	t.Run("倉庫別に在庫を取得できる", func(t *testing.T) {
		testDB := testutil.SetupTestDB(t)
		repo := NewStockRepository(testDB.DB)
		setupStockTestData(t, repo)

		stocks := []*model.Stock{
			{
				WarehouseCode:    "WH1",
				ProductCode:      "PRD001",
				LotNumber:        "LOT001",
				StockCategory:    "1",
				QualityCategory:  "A",
				ActualStock:      100,
				AvailableStock:   90,
				LastShippingDate: sql.NullTime{Valid: false},
				CreatedAt:        time.Now(),
				CreatedBy:        "admin",
				UpdatedAt:        time.Now(),
				UpdatedBy:        "admin",
			},
			{
				WarehouseCode:    "WH1",
				ProductCode:      "PRD002",
				LotNumber:        "LOT002",
				StockCategory:    "1",
				QualityCategory:  "A",
				ActualStock:      50,
				AvailableStock:   45,
				LastShippingDate: sql.NullTime{Valid: false},
				CreatedAt:        time.Now(),
				CreatedBy:        "admin",
				UpdatedAt:        time.Now(),
				UpdatedBy:        "admin",
			},
			{
				WarehouseCode:    "WH2",
				ProductCode:      "PRD001",
				LotNumber:        "LOT003",
				StockCategory:    "1",
				QualityCategory:  "A",
				ActualStock:      75,
				AvailableStock:   70,
				LastShippingDate: sql.NullTime{Valid: false},
				CreatedAt:        time.Now(),
				CreatedBy:        "admin",
				UpdatedAt:        time.Now(),
				UpdatedBy:        "admin",
			},
		}

		for _, stock := range stocks {
			err := repo.Create(stock)
			require.NoError(t, err)
		}

		found, err := repo.FindByWarehouse("WH1")
		require.NoError(t, err)
		assert.Equal(t, 2, len(found))
		for _, stock := range found {
			assert.Equal(t, "WH1", stock.WarehouseCode)
		}
	})
}

func TestStockRepository_FindByProduct(t *testing.T) {
	t.Run("商品別に在庫を取得できる", func(t *testing.T) {
		testDB := testutil.SetupTestDB(t)
		repo := NewStockRepository(testDB.DB)
		setupStockTestData(t, repo)

		stocks := []*model.Stock{
			{
				WarehouseCode:    "WH1",
				ProductCode:      "PRD001",
				LotNumber:        "LOT001",
				StockCategory:    "1",
				QualityCategory:  "A",
				ActualStock:      100,
				AvailableStock:   90,
				LastShippingDate: sql.NullTime{Valid: false},
				CreatedAt:        time.Now(),
				CreatedBy:        "admin",
				UpdatedAt:        time.Now(),
				UpdatedBy:        "admin",
			},
			{
				WarehouseCode:    "WH2",
				ProductCode:      "PRD001",
				LotNumber:        "LOT002",
				StockCategory:    "1",
				QualityCategory:  "A",
				ActualStock:      50,
				AvailableStock:   45,
				LastShippingDate: sql.NullTime{Valid: false},
				CreatedAt:        time.Now(),
				CreatedBy:        "admin",
				UpdatedAt:        time.Now(),
				UpdatedBy:        "admin",
			},
			{
				WarehouseCode:    "WH1",
				ProductCode:      "PRD002",
				LotNumber:        "LOT003",
				StockCategory:    "1",
				QualityCategory:  "A",
				ActualStock:      75,
				AvailableStock:   70,
				LastShippingDate: sql.NullTime{Valid: false},
				CreatedAt:        time.Now(),
				CreatedBy:        "admin",
				UpdatedAt:        time.Now(),
				UpdatedBy:        "admin",
			},
		}

		for _, stock := range stocks {
			err := repo.Create(stock)
			require.NoError(t, err)
		}

		found, err := repo.FindByProduct("PRD001")
		require.NoError(t, err)
		assert.Equal(t, 2, len(found))
		for _, stock := range found {
			assert.Equal(t, "PRD001", stock.ProductCode)
		}
	})
}

func TestStockRepository_Update(t *testing.T) {
	t.Run("在庫を更新できる", func(t *testing.T) {
		testDB := testutil.SetupTestDB(t)
		repo := NewStockRepository(testDB.DB)
		setupStockTestData(t, repo)

		stock := &model.Stock{
			WarehouseCode:    "WH1",
			ProductCode:      "PRD001",
			LotNumber:        "LOT001",
			StockCategory:    "1",
			QualityCategory:  "A",
			ActualStock:      100,
			AvailableStock:   90,
			LastShippingDate: sql.NullTime{Valid: false},
			CreatedAt:        time.Now(),
			CreatedBy:        "admin",
			UpdatedAt:        time.Now(),
			UpdatedBy:        "admin",
		}
		err := repo.Create(stock)
		require.NoError(t, err)

		// 在庫数を更新
		stock.ActualStock = 80
		stock.AvailableStock = 75
		stock.UpdatedAt = time.Now()
		stock.UpdatedBy = "admin2"

		err = repo.Update(stock)
		require.NoError(t, err)

		// 更新を確認
		found, err := repo.FindByID("WH1", "PRD001", "LOT001", "1", "A")
		require.NoError(t, err)
		assert.Equal(t, 80, found.ActualStock)
		assert.Equal(t, 75, found.AvailableStock)
		assert.Equal(t, "admin2", found.UpdatedBy)
	})
}

func TestStockRepository_Delete(t *testing.T) {
	t.Run("在庫を削除できる", func(t *testing.T) {
		testDB := testutil.SetupTestDB(t)
		repo := NewStockRepository(testDB.DB)
		setupStockTestData(t, repo)

		stock := &model.Stock{
			WarehouseCode:    "WH1",
			ProductCode:      "PRD001",
			LotNumber:        "LOT001",
			StockCategory:    "1",
			QualityCategory:  "A",
			ActualStock:      100,
			AvailableStock:   90,
			LastShippingDate: sql.NullTime{Valid: false},
			CreatedAt:        time.Now(),
			CreatedBy:        "admin",
			UpdatedAt:        time.Now(),
			UpdatedBy:        "admin",
		}
		err := repo.Create(stock)
		require.NoError(t, err)

		// 削除
		err = repo.Delete("WH1", "PRD001", "LOT001", "1", "A")
		require.NoError(t, err)

		// 削除を確認
		found, err := repo.FindByID("WH1", "PRD001", "LOT001", "1", "A")
		require.NoError(t, err)
		assert.Nil(t, found)
	})
}
