package repository

import (
	"testing"
	"time"

	"github.com/k2works/sales-management-db/internal/model"
	"github.com/k2works/sales-management-db/pkg/testutil"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestWarehouseRepository_Create(t *testing.T) {
	testDB := testutil.SetupTestDB(t)

	t.Run("倉庫を登録できる", func(t *testing.T) {
		repo := NewWarehouseRepository(testDB.DB)
		warehouse := &model.Warehouse{
			WarehouseCode: "WH1",
			WarehouseName: "東京倉庫",
			PostalCode:    "100-0001",
			Prefecture:    "東京都",
			Address1:      "千代田区丸の内1-1-1",
			Address2:      "ABCビル3F",
			PhoneNumber:   "03-1234-5678",
			FaxNumber:     "03-1234-5679",
			CreatedAt:     time.Now(),
			CreatedBy:     "admin",
			UpdatedAt:     time.Now(),
			UpdatedBy:     "admin",
		}

		err := repo.Create(warehouse)
		require.NoError(t, err)

		// 登録された倉庫を確認
		found, err := repo.FindByID("WH1")
		require.NoError(t, err)
		assert.Equal(t, "WH1", found.WarehouseCode)
		assert.Equal(t, "東京倉庫", found.WarehouseName)
		assert.Equal(t, "100-0001", found.PostalCode)
	})
}

func TestWarehouseRepository_FindAll(t *testing.T) {
	testDB := testutil.SetupTestDB(t)

	t.Run("全ての倉庫を取得できる", func(t *testing.T) {
		repo := NewWarehouseRepository(testDB.DB)
		warehouses := []*model.Warehouse{
			{
				WarehouseCode: "WH2",
				WarehouseName: "大阪倉庫",
				CreatedAt:     time.Now(),
				CreatedBy:     "admin",
				UpdatedAt:     time.Now(),
				UpdatedBy:     "admin",
			},
			{
				WarehouseCode: "WH3",
				WarehouseName: "名古屋倉庫",
				CreatedAt:     time.Now(),
				CreatedBy:     "admin",
				UpdatedAt:     time.Now(),
				UpdatedBy:     "admin",
			},
		}

		for _, wh := range warehouses {
			err := repo.Create(wh)
			require.NoError(t, err)
		}

		found, err := repo.FindAll()
		require.NoError(t, err)
		assert.GreaterOrEqual(t, len(found), 2)
	})
}

func TestWarehouseRepository_Update(t *testing.T) {
	testDB := testutil.SetupTestDB(t)

	t.Run("倉庫を更新できる", func(t *testing.T) {
		repo := NewWarehouseRepository(testDB.DB)
		warehouse := &model.Warehouse{
			WarehouseCode: "WH4",
			WarehouseName: "福岡倉庫",
			CreatedAt:     time.Now(),
			CreatedBy:     "admin",
			UpdatedAt:     time.Now(),
			UpdatedBy:     "admin",
		}
		err := repo.Create(warehouse)
		require.NoError(t, err)

		warehouse.WarehouseName = "福岡中央倉庫"
		warehouse.UpdatedAt = time.Now()
		warehouse.UpdatedBy = "admin2"

		err = repo.Update(warehouse)
		require.NoError(t, err)

		found, err := repo.FindByID("WH4")
		require.NoError(t, err)
		assert.Equal(t, "福岡中央倉庫", found.WarehouseName)
		assert.Equal(t, "admin2", found.UpdatedBy)
	})
}

func TestWarehouseRepository_Delete(t *testing.T) {
	testDB := testutil.SetupTestDB(t)

	t.Run("倉庫を削除できる", func(t *testing.T) {
		repo := NewWarehouseRepository(testDB.DB)
		warehouse := &model.Warehouse{
			WarehouseCode: "WH5",
			WarehouseName: "札幌倉庫",
			CreatedAt:     time.Now(),
			CreatedBy:     "admin",
			UpdatedAt:     time.Now(),
			UpdatedBy:     "admin",
		}
		err := repo.Create(warehouse)
		require.NoError(t, err)

		err = repo.Delete("WH5")
		require.NoError(t, err)

		found, err := repo.FindByID("WH5")
		require.NoError(t, err)
		assert.Nil(t, found)
	})
}
