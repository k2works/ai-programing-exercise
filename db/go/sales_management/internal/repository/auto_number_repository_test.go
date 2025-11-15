package repository

import (
	"testing"
	"time"

	"github.com/k2works/sales-management-db/internal/model"
	"github.com/k2works/sales-management-db/pkg/testutil"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestAutoNumberRepository_Create(t *testing.T) {
	testDB := testutil.SetupTestDB(t)

	t.Run("自動採番マスタを登録できる", func(t *testing.T) {
		// Given
		repo := NewAutoNumberRepository(testDB.DB)
		yearMonth := time.Date(2024, 1, 1, 0, 0, 0, 0, time.UTC)
		autoNumber := &model.AutoNumber{
			SlipType:       "OR",
			YearMonth:      yearMonth,
			LastSlipNumber: 0,
		}

		// When
		err := repo.Create(autoNumber)
		require.NoError(t, err)

		// Then
		retrieved, err := repo.FindByID("OR", yearMonth)
		require.NoError(t, err)
		require.NotNil(t, retrieved)
		assert.Equal(t, "OR", retrieved.SlipType)
		assert.Equal(t, 0, retrieved.LastSlipNumber)
	})
}

func TestAutoNumberRepository_GetNextNumber(t *testing.T) {
	testDB := testutil.SetupTestDB(t)

	t.Run("採番を進められる", func(t *testing.T) {
		// Given
		repo := NewAutoNumberRepository(testDB.DB)
		yearMonth := time.Date(2024, 1, 1, 0, 0, 0, 0, time.UTC)
		autoNumber := &model.AutoNumber{
			SlipType:       "OR",
			YearMonth:      yearMonth,
			LastSlipNumber: 0,
		}
		err := repo.Create(autoNumber)
		require.NoError(t, err)

		// When - トランザクション内で採番
		tx, err := testDB.DB.Beginx()
		require.NoError(t, err)
		defer tx.Rollback()

		newNo1, err := repo.GetNextNumber(tx, "OR", yearMonth)
		require.NoError(t, err)
		assert.Equal(t, 1, newNo1)

		newNo2, err := repo.GetNextNumber(tx, "OR", yearMonth)
		require.NoError(t, err)
		assert.Equal(t, 2, newNo2)

		err = tx.Commit()
		require.NoError(t, err)

		// Then - 確認
		retrieved, err := repo.FindByID("OR", yearMonth)
		require.NoError(t, err)
		assert.Equal(t, 2, retrieved.LastSlipNumber)
	})
}

func TestAutoNumberRepository_YearMonthReset(t *testing.T) {
	testDB := testutil.SetupTestDB(t)

	t.Run("年月が変わると番号がリセットされる", func(t *testing.T) {
		// Given
		repo := NewAutoNumberRepository(testDB.DB)

		// 1月の採番マスタ
		jan := time.Date(2024, 1, 1, 0, 0, 0, 0, time.UTC)
		janNumber := &model.AutoNumber{
			SlipType:       "OR",
			YearMonth:      jan,
			LastSlipNumber: 0,
		}
		err := repo.Create(janNumber)
		require.NoError(t, err)

		// 2月の採番マスタ
		feb := time.Date(2024, 2, 1, 0, 0, 0, 0, time.UTC)
		febNumber := &model.AutoNumber{
			SlipType:       "OR",
			YearMonth:      feb,
			LastSlipNumber: 0,
		}
		err = repo.Create(febNumber)
		require.NoError(t, err)

		// When - 2月の採番
		tx, err := testDB.DB.Beginx()
		require.NoError(t, err)
		defer tx.Rollback()

		newNo, err := repo.GetNextNumber(tx, "OR", feb)
		require.NoError(t, err)
		assert.Equal(t, 1, newNo) // 2月は1から始まる

		err = tx.Commit()
		require.NoError(t, err)
	})
}

func TestAutoNumberRepository_IndependentBySlipType(t *testing.T) {
	testDB := testutil.SetupTestDB(t)

	t.Run("伝票種別が異なれば独立して採番される", func(t *testing.T) {
		// Given
		repo := NewAutoNumberRepository(testDB.DB)
		yearMonth := time.Date(2024, 1, 1, 0, 0, 0, 0, time.UTC)

		// 受注伝票の採番マスタ
		orderNumber := &model.AutoNumber{
			SlipType:       "OR",
			YearMonth:      yearMonth,
			LastSlipNumber: 0,
		}
		err := repo.Create(orderNumber)
		require.NoError(t, err)

		// 売上伝票の採番マスタ
		salesNumber := &model.AutoNumber{
			SlipType:       "SL",
			YearMonth:      yearMonth,
			LastSlipNumber: 0,
		}
		err = repo.Create(salesNumber)
		require.NoError(t, err)

		// When - それぞれの伝票種別で採番
		tx, err := testDB.DB.Beginx()
		require.NoError(t, err)
		defer tx.Rollback()

		// 受注番号
		orNo1, err := repo.GetNextNumber(tx, "OR", yearMonth)
		require.NoError(t, err)
		assert.Equal(t, 1, orNo1)

		orNo2, err := repo.GetNextNumber(tx, "OR", yearMonth)
		require.NoError(t, err)
		assert.Equal(t, 2, orNo2)

		// 売上番号
		slNo1, err := repo.GetNextNumber(tx, "SL", yearMonth)
		require.NoError(t, err)
		assert.Equal(t, 1, slNo1) // SLは独立して1から

		err = tx.Commit()
		require.NoError(t, err)

		// Then - 確認
		orRetrieved, err := repo.FindByID("OR", yearMonth)
		require.NoError(t, err)
		assert.Equal(t, 2, orRetrieved.LastSlipNumber)

		slRetrieved, err := repo.FindByID("SL", yearMonth)
		require.NoError(t, err)
		assert.Equal(t, 1, slRetrieved.LastSlipNumber)
	})
}
