package repository

import (
	"testing"
	"time"

	"github.com/k2works/sales-management-db/internal/model"
	"github.com/k2works/sales-management-db/pkg/testutil"
	"github.com/k2works/sales-management-db/test"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestDepartmentRepository_Create(t *testing.T) {
	testDB := testutil.SetupTestDB(t)

	// マイグレーション実行
	migrations := []string{
		`CREATE TABLE 部門マスタ (
			部門コード VARCHAR(6) NOT NULL,
			開始日 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
			終了日 TIMESTAMP DEFAULT '2100-12-31 00:00:00',
			部門名 VARCHAR(40),
			組織階層 INT DEFAULT 0,
			部門パス VARCHAR(100) NOT NULL,
			最下層区分 INT DEFAULT 0,
			伝票入力可否 INT DEFAULT 0,
			作成日時 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
			作成者名 VARCHAR(50) NOT NULL,
			更新日時 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
			更新者名 VARCHAR(50) NOT NULL,
			PRIMARY KEY (部門コード, 開始日)
		)`,
	}
	test.RunMigrations(t, testDB.DB, migrations)

	t.Run("部門を登録できる", func(t *testing.T) {
		// Given: 部門の情報が与えられた時
		dept := &model.Department{
			DepartmentCode: "11101",
			StartDate:      time.Date(2021, 1, 1, 0, 0, 0, 0, time.UTC),
			EndDate:        time.Date(2100, 12, 31, 0, 0, 0, 0, time.UTC),
			DepartmentName: "新規部署",
			HierarchyLevel: 1,
			DepartmentPath: "D0001",
			IsLowestLevel:  1,
			CanEnterSlip:   0,
			CreatedAt:      time.Now(),
			CreatedBy:      "admin",
			UpdatedAt:      time.Now(),
			UpdatedBy:      "admin",
		}

		// When: 部門を登録すると
		repo := NewDepartmentRepository(testDB.DB)
		err := repo.Create(dept)

		// Then: エラーなく登録できる
		require.NoError(t, err)

		// 登録された部門を確認
		found, err := repo.FindByID(dept.DepartmentCode, dept.StartDate)
		require.NoError(t, err)
		assert.Equal(t, "11101", found.DepartmentCode)
		assert.Equal(t, "新規部署", found.DepartmentName)
	})
}

func TestDepartmentRepository_FindAll(t *testing.T) {
	testDB := testutil.SetupTestDB(t)

	// マイグレーション実行
	migrations := []string{
		`CREATE TABLE IF NOT EXISTS 部門マスタ (
			部門コード VARCHAR(6) NOT NULL,
			開始日 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
			終了日 TIMESTAMP DEFAULT '2100-12-31 00:00:00',
			部門名 VARCHAR(40),
			組織階層 INT DEFAULT 0,
			部門パス VARCHAR(100) NOT NULL,
			最下層区分 INT DEFAULT 0,
			伝票入力可否 INT DEFAULT 0,
			作成日時 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
			作成者名 VARCHAR(50) NOT NULL,
			更新日時 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
			更新者名 VARCHAR(50) NOT NULL,
			PRIMARY KEY (部門コード, 開始日)
		)`,
	}
	test.RunMigrations(t, testDB.DB, migrations)

	t.Run("すべての部門を取得できる", func(t *testing.T) {
		// Given: 複数の部門が登録されている時
		repo := NewDepartmentRepository(testDB.DB)
		depts := []*model.Department{
			{
				DepartmentCode: "11101",
				StartDate:      time.Date(2021, 1, 1, 0, 0, 0, 0, time.UTC),
				EndDate:        time.Date(2100, 12, 31, 0, 0, 0, 0, time.UTC),
				DepartmentName: "営業部",
				HierarchyLevel: 1,
				DepartmentPath: "/11101/",
				IsLowestLevel:  1,
				CanEnterSlip:   1,
				CreatedAt:      time.Now(),
				CreatedBy:      "admin",
				UpdatedAt:      time.Now(),
				UpdatedBy:      "admin",
			},
			{
				DepartmentCode: "11102",
				StartDate:      time.Date(2021, 1, 1, 0, 0, 0, 0, time.UTC),
				EndDate:        time.Date(2100, 12, 31, 0, 0, 0, 0, time.UTC),
				DepartmentName: "開発部",
				HierarchyLevel: 1,
				DepartmentPath: "/11102/",
				IsLowestLevel:  1,
				CanEnterSlip:   1,
				CreatedAt:      time.Now(),
				CreatedBy:      "admin",
				UpdatedAt:      time.Now(),
				UpdatedBy:      "admin",
			},
		}
		for _, d := range depts {
			err := repo.Create(d)
			require.NoError(t, err)
		}

		// When: すべての部門を取得すると
		found, err := repo.FindAll()

		// Then: 登録した部門がすべて取得できる
		require.NoError(t, err)
		assert.GreaterOrEqual(t, len(found), 2)
	})
}

func TestDepartmentRepository_Update(t *testing.T) {
	testDB := testutil.SetupTestDB(t)

	// マイグレーション実行
	migrations := []string{
		`CREATE TABLE IF NOT EXISTS 部門マスタ (
			部門コード VARCHAR(6) NOT NULL,
			開始日 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
			終了日 TIMESTAMP DEFAULT '2100-12-31 00:00:00',
			部門名 VARCHAR(40),
			組織階層 INT DEFAULT 0,
			部門パス VARCHAR(100) NOT NULL,
			最下層区分 INT DEFAULT 0,
			伝票入力可否 INT DEFAULT 0,
			作成日時 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
			作成者名 VARCHAR(50) NOT NULL,
			更新日時 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
			更新者名 VARCHAR(50) NOT NULL,
			PRIMARY KEY (部門コード, 開始日)
		)`,
	}
	test.RunMigrations(t, testDB.DB, migrations)

	t.Run("部門を更新できる", func(t *testing.T) {
		// Given: 部門が登録されている時
		repo := NewDepartmentRepository(testDB.DB)
		dept := &model.Department{
			DepartmentCode: "11103",
			StartDate:      time.Date(2021, 1, 1, 0, 0, 0, 0, time.UTC),
			EndDate:        time.Date(2100, 12, 31, 0, 0, 0, 0, time.UTC),
			DepartmentName: "更新前部署",
			HierarchyLevel: 1,
			DepartmentPath: "/11103/",
			IsLowestLevel:  1,
			CanEnterSlip:   0,
			CreatedAt:      time.Now(),
			CreatedBy:      "admin",
			UpdatedAt:      time.Now(),
			UpdatedBy:      "admin",
		}
		err := repo.Create(dept)
		require.NoError(t, err)

		// When: 部門を更新すると
		dept.DepartmentName = "更新後部署"
		dept.UpdatedAt = time.Now()
		err = repo.Update(dept)
		require.NoError(t, err)

		// Then: 部門が更新されている
		found, err := repo.FindByID("11103", dept.StartDate)
		require.NoError(t, err)
		assert.Equal(t, "更新後部署", found.DepartmentName)
	})
}

func TestDepartmentRepository_Delete(t *testing.T) {
	testDB := testutil.SetupTestDB(t)

	// マイグレーション実行
	migrations := []string{
		`CREATE TABLE IF NOT EXISTS 部門マスタ (
			部門コード VARCHAR(6) NOT NULL,
			開始日 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
			終了日 TIMESTAMP DEFAULT '2100-12-31 00:00:00',
			部門名 VARCHAR(40),
			組織階層 INT DEFAULT 0,
			部門パス VARCHAR(100) NOT NULL,
			最下層区分 INT DEFAULT 0,
			伝票入力可否 INT DEFAULT 0,
			作成日時 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
			作成者名 VARCHAR(50) NOT NULL,
			更新日時 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
			更新者名 VARCHAR(50) NOT NULL,
			PRIMARY KEY (部門コード, 開始日)
		)`,
	}
	test.RunMigrations(t, testDB.DB, migrations)

	t.Run("部門を削除できる", func(t *testing.T) {
		// Given: 部門が登録されている時
		repo := NewDepartmentRepository(testDB.DB)
		dept := &model.Department{
			DepartmentCode: "11104",
			StartDate:      time.Date(2021, 1, 1, 0, 0, 0, 0, time.UTC),
			EndDate:        time.Date(2100, 12, 31, 0, 0, 0, 0, time.UTC),
			DepartmentName: "削除テスト部署",
			HierarchyLevel: 1,
			DepartmentPath: "/11104/",
			IsLowestLevel:  1,
			CanEnterSlip:   0,
			CreatedAt:      time.Now(),
			CreatedBy:      "admin",
			UpdatedAt:      time.Now(),
			UpdatedBy:      "admin",
		}
		err := repo.Create(dept)
		require.NoError(t, err)

		// When: 部門を削除すると
		err = repo.Delete("11104", dept.StartDate)
		require.NoError(t, err)

		// Then: 部門が削除されている
		found, err := repo.FindByID("11104", dept.StartDate)
		require.NoError(t, err)
		assert.Nil(t, found)
	})
}
