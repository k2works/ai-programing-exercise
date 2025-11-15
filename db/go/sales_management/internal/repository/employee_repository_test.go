package repository

import (
	"testing"
	"time"

	"github.com/k2works/sales-management-db/internal/model"
	"github.com/k2works/sales-management-db/pkg/testutil"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestEmployeeRepository_Create(t *testing.T) {
	testDB := testutil.SetupTestDB(t)

	t.Run("社員を登録できる", func(t *testing.T) {
		// Given: 部門が存在する時
		deptRepo := NewDepartmentRepository(testDB.DB)
		dept := &model.Department{
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
		}
		err := deptRepo.Create(dept)
		require.NoError(t, err)

		emp := &model.Employee{
			EmployeeCode:          "E001",
			EmployeeName:          "山田太郎",
			EmployeeNameKana:      "ヤマダタロウ",
			Password:              "pass123",
			PhoneNumber:           "03-1234-5678",
			FaxNumber:             "03-1234-5679",
			DepartmentCode:        "11101",
			StartDate:             time.Date(2021, 4, 1, 0, 0, 0, 0, time.UTC),
			JobTypeCode:           "01",
			ApprovalAuthorityCode: "02",
			CreatedAt:             time.Now(),
			CreatedBy:             "admin",
			UpdatedAt:             time.Now(),
			UpdatedBy:             "admin",
		}

		// When: 社員を登録すると
		repo := NewEmployeeRepository(testDB.DB)
		err = repo.Create(emp)

		// Then: エラーなく登録できる
		require.NoError(t, err)

		// 登録された社員を確認
		found, err := repo.FindByID(emp.EmployeeCode)
		require.NoError(t, err)
		assert.Equal(t, "E001", found.EmployeeCode)
		assert.Equal(t, "山田太郎", found.EmployeeName)
	})
}

func TestEmployeeRepository_FindByDepartment(t *testing.T) {
	testDB := testutil.SetupTestDB(t)

	t.Run("部門別に社員を検索できる", func(t *testing.T) {
		// Given: 複数の部門と社員が登録されている時
		deptRepo := NewDepartmentRepository(testDB.DB)
		dept1 := &model.Department{
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
		}
		dept2 := &model.Department{
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
		}
		err := deptRepo.Create(dept1)
		require.NoError(t, err)
		err = deptRepo.Create(dept2)
		require.NoError(t, err)

		empRepo := NewEmployeeRepository(testDB.DB)
		employees := []*model.Employee{
			{
				EmployeeCode:          "E001",
				EmployeeName:          "営業太郎",
				EmployeeNameKana:      "エイギョウタロウ",
				Password:              "pass123",
				PhoneNumber:           "03-1234-5678",
				FaxNumber:             "03-1234-5679",
				DepartmentCode:        "11101",
				StartDate:             time.Date(2021, 4, 1, 0, 0, 0, 0, time.UTC),
				JobTypeCode:           "01",
				ApprovalAuthorityCode: "02",
				CreatedAt:             time.Now(),
				CreatedBy:             "admin",
				UpdatedAt:             time.Now(),
				UpdatedBy:             "admin",
			},
			{
				EmployeeCode:          "E002",
				EmployeeName:          "営業花子",
				EmployeeNameKana:      "エイギョウハナコ",
				Password:              "pass456",
				PhoneNumber:           "03-2234-5678",
				FaxNumber:             "03-2234-5679",
				DepartmentCode:        "11101",
				StartDate:             time.Date(2021, 4, 1, 0, 0, 0, 0, time.UTC),
				JobTypeCode:           "01",
				ApprovalAuthorityCode: "02",
				CreatedAt:             time.Now(),
				CreatedBy:             "admin",
				UpdatedAt:             time.Now(),
				UpdatedBy:             "admin",
			},
			{
				EmployeeCode:          "E003",
				EmployeeName:          "開発次郎",
				EmployeeNameKana:      "カイハツジロウ",
				Password:              "pass789",
				PhoneNumber:           "03-3234-5678",
				FaxNumber:             "03-3234-5679",
				DepartmentCode:        "11102",
				StartDate:             time.Date(2021, 4, 1, 0, 0, 0, 0, time.UTC),
				JobTypeCode:           "02",
				ApprovalAuthorityCode: "01",
				CreatedAt:             time.Now(),
				CreatedBy:             "admin",
				UpdatedAt:             time.Now(),
				UpdatedBy:             "admin",
			},
		}
		for _, e := range employees {
			err := empRepo.Create(e)
			require.NoError(t, err)
		}

		// When: 営業部の社員を検索すると
		found, err := empRepo.FindByDepartment("11101")

		// Then: 営業部の社員のみ取得できる
		require.NoError(t, err)
		assert.Equal(t, 2, len(found))
		assert.Equal(t, "営業太郎", found[0].EmployeeName)
		assert.Equal(t, "営業花子", found[1].EmployeeName)
	})
}

func TestEmployeeRepository_FindAll(t *testing.T) {
	testDB := testutil.SetupTestDB(t)

	t.Run("すべての社員を取得できる", func(t *testing.T) {
		// Given: 部門と複数の社員が登録されている時
		deptRepo := NewDepartmentRepository(testDB.DB)
		dept := &model.Department{
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
		}
		err := deptRepo.Create(dept)
		require.NoError(t, err)

		empRepo := NewEmployeeRepository(testDB.DB)
		employees := []*model.Employee{
			{
				EmployeeCode:          "E001",
				EmployeeName:          "山田太郎",
				EmployeeNameKana:      "ヤマダタロウ",
				Password:              "pass123",
				PhoneNumber:           "03-1234-5678",
				FaxNumber:             "03-1234-5679",
				DepartmentCode:        "11101",
				StartDate:             time.Date(2021, 4, 1, 0, 0, 0, 0, time.UTC),
				JobTypeCode:           "01",
				ApprovalAuthorityCode: "02",
				CreatedAt:             time.Now(),
				CreatedBy:             "admin",
				UpdatedAt:             time.Now(),
				UpdatedBy:             "admin",
			},
			{
				EmployeeCode:          "E002",
				EmployeeName:          "佐藤花子",
				EmployeeNameKana:      "サトウハナコ",
				Password:              "pass456",
				PhoneNumber:           "03-2234-5678",
				FaxNumber:             "03-2234-5679",
				DepartmentCode:        "11101",
				StartDate:             time.Date(2021, 4, 1, 0, 0, 0, 0, time.UTC),
				JobTypeCode:           "01",
				ApprovalAuthorityCode: "02",
				CreatedAt:             time.Now(),
				CreatedBy:             "admin",
				UpdatedAt:             time.Now(),
				UpdatedBy:             "admin",
			},
		}
		for _, e := range employees {
			err := empRepo.Create(e)
			require.NoError(t, err)
		}

		// When: すべての社員を取得すると
		found, err := empRepo.FindAll()

		// Then: 登録した社員がすべて取得できる
		require.NoError(t, err)
		assert.GreaterOrEqual(t, len(found), 2)
	})
}

func TestEmployeeRepository_Update(t *testing.T) {
	testDB := testutil.SetupTestDB(t)

	t.Run("社員を更新できる", func(t *testing.T) {
		// Given: 部門と社員が登録されている時
		deptRepo := NewDepartmentRepository(testDB.DB)
		dept := &model.Department{
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
		}
		err := deptRepo.Create(dept)
		require.NoError(t, err)

		empRepo := NewEmployeeRepository(testDB.DB)
		emp := &model.Employee{
			EmployeeCode:          "E001",
			EmployeeName:          "更新前太郎",
			EmployeeNameKana:      "コウシンマエタロウ",
			Password:              "pass123",
			PhoneNumber:           "03-1234-5678",
			FaxNumber:             "03-1234-5679",
			DepartmentCode:        "11101",
			StartDate:             time.Date(2021, 4, 1, 0, 0, 0, 0, time.UTC),
			JobTypeCode:           "01",
			ApprovalAuthorityCode: "02",
			CreatedAt:             time.Now(),
			CreatedBy:             "admin",
			UpdatedAt:             time.Now(),
			UpdatedBy:             "admin",
		}
		err = empRepo.Create(emp)
		require.NoError(t, err)

		// When: 社員を更新すると
		emp.EmployeeName = "更新後太郎"
		emp.UpdatedAt = time.Now()
		err = empRepo.Update(emp)
		require.NoError(t, err)

		// Then: 社員が更新されている
		found, err := empRepo.FindByID("E001")
		require.NoError(t, err)
		assert.Equal(t, "更新後太郎", found.EmployeeName)
	})
}

func TestEmployeeRepository_Delete(t *testing.T) {
	testDB := testutil.SetupTestDB(t)

	t.Run("社員を削除できる", func(t *testing.T) {
		// Given: 部門と社員が登録されている時
		deptRepo := NewDepartmentRepository(testDB.DB)
		dept := &model.Department{
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
		}
		err := deptRepo.Create(dept)
		require.NoError(t, err)

		empRepo := NewEmployeeRepository(testDB.DB)
		emp := &model.Employee{
			EmployeeCode:          "E001",
			EmployeeName:          "削除テスト太郎",
			EmployeeNameKana:      "サクジョテストタロウ",
			Password:              "pass123",
			PhoneNumber:           "03-1234-5678",
			FaxNumber:             "03-1234-5679",
			DepartmentCode:        "11101",
			StartDate:             time.Date(2021, 4, 1, 0, 0, 0, 0, time.UTC),
			JobTypeCode:           "01",
			ApprovalAuthorityCode: "02",
			CreatedAt:             time.Now(),
			CreatedBy:             "admin",
			UpdatedAt:             time.Now(),
			UpdatedBy:             "admin",
		}
		err = empRepo.Create(emp)
		require.NoError(t, err)

		// When: 社員を削除すると
		err = empRepo.Delete("E001")
		require.NoError(t, err)

		// Then: 社員が削除されている
		found, err := empRepo.FindByID("E001")
		require.NoError(t, err)
		assert.Nil(t, found)
	})
}
