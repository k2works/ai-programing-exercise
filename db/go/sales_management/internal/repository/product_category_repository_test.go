package repository

import (
	"testing"
	"time"

	"github.com/k2works/sales-management-db/internal/model"
	"github.com/k2works/sales-management-db/pkg/testutil"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestProductCategoryRepository_Create(t *testing.T) {
	testDB := testutil.SetupTestDB(t)

	t.Run("商品分類を登録できる", func(t *testing.T) {
		// Given: 新しい商品分類を作成
		repo := NewProductCategoryRepository(testDB.DB)
		category := &model.ProductCategory{
			CategoryCode:  "PC",
			CategoryName:  "パソコン",
			CategoryLevel: 1,
			CategoryPath:  "/PC/",
			IsLowestLevel: 0,
			CreatedAt:     time.Now(),
			CreatedBy:     "admin",
			UpdatedAt:     time.Now(),
			UpdatedBy:     "admin",
		}

		// When: 商品分類を登録すると
		err := repo.Create(category)

		// Then: エラーなく登録できる
		require.NoError(t, err)

		// 登録された商品分類を確認
		found, err := repo.FindByID("PC")
		require.NoError(t, err)
		assert.Equal(t, "PC", found.CategoryCode)
		assert.Equal(t, "パソコン", found.CategoryName)
		assert.Equal(t, 1, found.CategoryLevel)
	})
}

func TestProductCategoryRepository_FindAll(t *testing.T) {
	testDB := testutil.SetupTestDB(t)

	t.Run("すべての商品分類を取得できる", func(t *testing.T) {
		// Given: 複数の商品分類が登録されている
		repo := NewProductCategoryRepository(testDB.DB)
		categories := []*model.ProductCategory{
			{
				CategoryCode:  "PC",
				CategoryName:  "パソコン",
				CategoryLevel: 1,
				CategoryPath:  "/PC/",
				IsLowestLevel: 0,
				CreatedAt:     time.Now(),
				CreatedBy:     "admin",
				UpdatedAt:     time.Now(),
				UpdatedBy:     "admin",
			},
			{
				CategoryCode:  "SERVER",
				CategoryName:  "サーバー",
				CategoryLevel: 1,
				CategoryPath:  "/SERVER/",
				IsLowestLevel: 0,
				CreatedAt:     time.Now(),
				CreatedBy:     "admin",
				UpdatedAt:     time.Now(),
				UpdatedBy:     "admin",
			},
		}

		for _, c := range categories {
			err := repo.Create(c)
			require.NoError(t, err)
		}

		// When: すべての商品分類を取得すると
		found, err := repo.FindAll()

		// Then: 登録した商品分類がすべて取得できる
		require.NoError(t, err)
		assert.Len(t, found, 2)
	})
}

func TestProductCategoryRepository_FindByLevel(t *testing.T) {
	testDB := testutil.SetupTestDB(t)

	t.Run("階層レベルで商品分類を取得できる", func(t *testing.T) {
		// Given: 複数階層の商品分類が登録されている
		repo := NewProductCategoryRepository(testDB.DB)
		categories := []*model.ProductCategory{
			{
				CategoryCode:  "PC",
				CategoryName:  "パソコン",
				CategoryLevel: 1,
				CategoryPath:  "/PC/",
				IsLowestLevel: 0,
				CreatedAt:     time.Now(),
				CreatedBy:     "admin",
				UpdatedAt:     time.Now(),
				UpdatedBy:     "admin",
			},
			{
				CategoryCode:  "PC01",
				CategoryName:  "ノートPC",
				CategoryLevel: 2,
				CategoryPath:  "/PC/PC01/",
				IsLowestLevel: 1,
				CreatedAt:     time.Now(),
				CreatedBy:     "admin",
				UpdatedAt:     time.Now(),
				UpdatedBy:     "admin",
			},
			{
				CategoryCode:  "PC02",
				CategoryName:  "デスクトップPC",
				CategoryLevel: 2,
				CategoryPath:  "/PC/PC02/",
				IsLowestLevel: 1,
				CreatedAt:     time.Now(),
				CreatedBy:     "admin",
				UpdatedAt:     time.Now(),
				UpdatedBy:     "admin",
			},
		}

		for _, c := range categories {
			err := repo.Create(c)
			require.NoError(t, err)
		}

		// When: レベル2の商品分類を取得すると
		found, err := repo.FindByLevel(2)

		// Then: レベル2の商品分類のみ取得できる
		require.NoError(t, err)
		assert.Len(t, found, 2)
		assert.Equal(t, 2, found[0].CategoryLevel)
		assert.Equal(t, 2, found[1].CategoryLevel)
	})
}

func TestProductCategoryRepository_Update(t *testing.T) {
	testDB := testutil.SetupTestDB(t)

	t.Run("商品分類を更新できる", func(t *testing.T) {
		// Given: 商品分類が登録されている
		repo := NewProductCategoryRepository(testDB.DB)
		category := &model.ProductCategory{
			CategoryCode:  "PC",
			CategoryName:  "パソコン",
			CategoryLevel: 1,
			CategoryPath:  "/PC/",
			IsLowestLevel: 0,
			CreatedAt:     time.Now(),
			CreatedBy:     "admin",
			UpdatedAt:     time.Now(),
			UpdatedBy:     "admin",
		}
		err := repo.Create(category)
		require.NoError(t, err)

		// When: 商品分類情報を更新すると
		category.CategoryName = "コンピュータ"
		category.UpdatedAt = time.Now()
		category.UpdatedBy = "user1"
		err = repo.Update(category)

		// Then: 更新が反映される
		require.NoError(t, err)

		found, err := repo.FindByID("PC")
		require.NoError(t, err)
		assert.Equal(t, "コンピュータ", found.CategoryName)
		assert.Equal(t, "user1", found.UpdatedBy)
	})
}

func TestProductCategoryRepository_Delete(t *testing.T) {
	testDB := testutil.SetupTestDB(t)

	t.Run("商品分類を削除できる", func(t *testing.T) {
		// Given: 商品分類が登録されている
		repo := NewProductCategoryRepository(testDB.DB)
		category := &model.ProductCategory{
			CategoryCode:  "PC",
			CategoryName:  "パソコン",
			CategoryLevel: 1,
			CategoryPath:  "/PC/",
			IsLowestLevel: 0,
			CreatedAt:     time.Now(),
			CreatedBy:     "admin",
			UpdatedAt:     time.Now(),
			UpdatedBy:     "admin",
		}
		err := repo.Create(category)
		require.NoError(t, err)

		// When: 商品分類を削除すると
		err = repo.Delete("PC")

		// Then: 削除される
		require.NoError(t, err)

		found, err := repo.FindByID("PC")
		require.NoError(t, err)
		assert.Nil(t, found)
	})
}
