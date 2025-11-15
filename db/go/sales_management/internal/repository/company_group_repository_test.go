package repository

import (
	"testing"
	"time"

	"github.com/k2works/sales-management-db/internal/model"
	"github.com/k2works/sales-management-db/pkg/testutil"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestCompanyGroupRepository_Create(t *testing.T) {
	testDB := testutil.SetupTestDB(t)

	t.Run("取引先グループを登録できる", func(t *testing.T) {
		// Given: 新しい取引先グループを作成
		repo := NewCompanyGroupRepository(testDB.DB)
		group := &model.CompanyGroup{
			GroupCode: "GRP1",
			GroupName: "優良企業グループ",
			CreatedAt: time.Now(),
			CreatedBy: "admin",
			UpdatedAt: time.Now(),
			UpdatedBy: "admin",
		}

		// When: 取引先グループを登録すると
		err := repo.Create(group)

		// Then: エラーなく登録できる
		require.NoError(t, err)

		// 登録された取引先グループを確認
		found, err := repo.FindByID("GRP1")
		require.NoError(t, err)
		assert.Equal(t, "GRP1", found.GroupCode)
		assert.Equal(t, "優良企業グループ", found.GroupName)
	})
}

func TestCompanyGroupRepository_FindAll(t *testing.T) {
	testDB := testutil.SetupTestDB(t)

	t.Run("すべての取引先グループを取得できる", func(t *testing.T) {
		// Given: 複数の取引先グループが登録されている
		repo := NewCompanyGroupRepository(testDB.DB)
		groups := []*model.CompanyGroup{
			{
				GroupCode: "GRP1",
				GroupName: "優良企業グループ",
				CreatedAt: time.Now(),
				CreatedBy: "admin",
				UpdatedAt: time.Now(),
				UpdatedBy: "admin",
			},
			{
				GroupCode: "GRP2",
				GroupName: "一般企業グループ",
				CreatedAt: time.Now(),
				CreatedBy: "admin",
				UpdatedAt: time.Now(),
				UpdatedBy: "admin",
			},
		}

		for _, g := range groups {
			err := repo.Create(g)
			require.NoError(t, err)
		}

		// When: すべての取引先グループを取得すると
		found, err := repo.FindAll()

		// Then: 登録した取引先グループがすべて取得できる
		require.NoError(t, err)
		assert.Len(t, found, 2)
	})
}

func TestCompanyGroupRepository_Update(t *testing.T) {
	testDB := testutil.SetupTestDB(t)

	t.Run("取引先グループを更新できる", func(t *testing.T) {
		// Given: 取引先グループが登録されている
		repo := NewCompanyGroupRepository(testDB.DB)
		group := &model.CompanyGroup{
			GroupCode: "GRP1",
			GroupName: "優良企業グループ",
			CreatedAt: time.Now(),
			CreatedBy: "admin",
			UpdatedAt: time.Now(),
			UpdatedBy: "admin",
		}
		err := repo.Create(group)
		require.NoError(t, err)

		// When: 取引先グループ情報を更新すると
		group.GroupName = "最優良企業グループ"
		group.UpdatedAt = time.Now()
		group.UpdatedBy = "user1"
		err = repo.Update(group)

		// Then: 更新が反映される
		require.NoError(t, err)

		found, err := repo.FindByID("GRP1")
		require.NoError(t, err)
		assert.Equal(t, "最優良企業グループ", found.GroupName)
		assert.Equal(t, "user1", found.UpdatedBy)
	})
}

func TestCompanyGroupRepository_Delete(t *testing.T) {
	testDB := testutil.SetupTestDB(t)

	t.Run("取引先グループを削除できる", func(t *testing.T) {
		// Given: 取引先グループが登録されている
		repo := NewCompanyGroupRepository(testDB.DB)
		group := &model.CompanyGroup{
			GroupCode: "GRP1",
			GroupName: "優良企業グループ",
			CreatedAt: time.Now(),
			CreatedBy: "admin",
			UpdatedAt: time.Now(),
			UpdatedBy: "admin",
		}
		err := repo.Create(group)
		require.NoError(t, err)

		// When: 取引先グループを削除すると
		err = repo.Delete("GRP1")

		// Then: 削除される
		require.NoError(t, err)

		found, err := repo.FindByID("GRP1")
		require.NoError(t, err)
		assert.Nil(t, found)
	})
}
