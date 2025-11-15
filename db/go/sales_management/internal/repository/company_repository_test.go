package repository

import (
	"testing"
	"time"

	"github.com/k2works/sales-management-db/internal/model"
	"github.com/k2works/sales-management-db/pkg/testutil"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestCompanyRepository_Create(t *testing.T) {
	testDB := testutil.SetupTestDB(t)

	t.Run("取引先を登録できる", func(t *testing.T) {
		// Given: 取引先グループを先に作成
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

		// When: 取引先を登録すると
		repo := NewCompanyRepository(testDB.DB)
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
		err = repo.Create(company)

		// Then: エラーなく登録できる
		require.NoError(t, err)

		// 登録された取引先を確認
		found, err := repo.FindByID("COMP001")
		require.NoError(t, err)
		assert.Equal(t, "COMP001", found.CompanyCode)
		assert.Equal(t, "株式会社ABC", found.CompanyName)
	})
}
