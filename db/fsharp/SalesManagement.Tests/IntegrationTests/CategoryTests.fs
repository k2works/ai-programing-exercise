module SalesManagement.Tests.IntegrationTests.CategoryTests

open System
open Xunit
open FsUnit.Xunit
open SalesManagement.Domain.Models
open SalesManagement.Infrastructure.Repositories
open SalesManagement.Tests.DatabaseTestBase

type CategoryTests() =
    inherit DatabaseTestBase()

    let setupCompany (connectionString: string) companyCode =
        task {
            let group = {
                CompanyGroupCode = "GRP1"
                CompanyGroupName = "テストグループ"
                CreatedAt = DateTime.Now
                CreatedBy = "admin"
                UpdatedAt = DateTime.Now
                UpdatedBy = "admin"
            }
            do! CompanyGroupRepository.insertAsync connectionString group

            let company = {
                CompanyCode = companyCode
                CompanyName = "テスト商事"
                CompanyNameKana = None
                SupplierType = 0
                ZipCode = None
                State = None
                Address1 = None
                Address2 = None
                NoSalesFlag = 0
                WideUseType = 0
                CompanyGroupCode = "GRP1"
                MaxCredit = 0
                TempCreditUp = 0
                CreatedAt = DateTime.Now
                CreatedBy = "admin"
                UpdatedAt = DateTime.Now
                UpdatedBy = "admin"
            }
            do! CompanyRepository.insertAsync connectionString company
        }

    let createTestCategoryType code name =
        {
            CategoryTypeCode = code
            CategoryTypeName = name
            CreatedAt = DateTime.Now
            CreatedBy = "admin"
            UpdatedAt = DateTime.Now
            UpdatedBy = "admin"
        }

    let createTestCompanyCategory typeCode code name =
        {
            CategoryTypeCode = typeCode
            CategoryCode = code
            CategoryName = name
            CreatedAt = DateTime.Now
            CreatedBy = "admin"
            UpdatedAt = DateTime.Now
            UpdatedBy = "admin"
        }

    let createTestCategoryGroup typeCode categoryCode companyCode =
        {
            CategoryTypeCode = typeCode
            CategoryCode = categoryCode
            CompanyCode = companyCode
            CreatedAt = DateTime.Now
            CreatedBy = "admin"
            UpdatedAt = DateTime.Now
            UpdatedBy = "admin"
        }

    // ========================================
    // 分類種別マスタのテスト
    // ========================================

    [<Fact>]
    member this.``分類種別を登録できる``() =
        task {
            // Arrange
            let categoryType = createTestCategoryType "IND" "業種"

            // Act
            do! CategoryTypeRepository.insertAsync this.ConnectionString categoryType

            // Assert
            let! found = CategoryTypeRepository.findByIdAsync this.ConnectionString "IND"
            found |> should not' (equal None)
            found.Value.CategoryTypeName |> should equal "業種"
        }

    [<Fact>]
    member this.``分類種別を更新できる``() =
        task {
            // Arrange
            let categoryType = createTestCategoryType "IND" "業種"
            do! CategoryTypeRepository.insertAsync this.ConnectionString categoryType

            // Act
            let updated =
                { categoryType with
                    CategoryTypeName = "業種分類"
                    UpdatedAt = DateTime.Now
                    UpdatedBy = "updater" }
            do! CategoryTypeRepository.updateAsync this.ConnectionString updated

            // Assert
            let! result = CategoryTypeRepository.findByIdAsync this.ConnectionString "IND"
            result |> should not' (equal None)
            result.Value.CategoryTypeName |> should equal "業種分類"
        }

    [<Fact>]
    member this.``分類種別を削除できる``() =
        task {
            // Arrange
            let categoryType = createTestCategoryType "IND" "業種"
            do! CategoryTypeRepository.insertAsync this.ConnectionString categoryType

            // Act
            do! CategoryTypeRepository.deleteAsync this.ConnectionString "IND"

            // Assert
            let! found = CategoryTypeRepository.findByIdAsync this.ConnectionString "IND"
            found |> should equal None
        }

    // ========================================
    // 取引先分類マスタのテスト
    // ========================================

    [<Fact>]
    member this.``取引先分類を登録できる``() =
        task {
            // Arrange
            let categoryType = createTestCategoryType "IND" "業種"
            do! CategoryTypeRepository.insertAsync this.ConnectionString categoryType

            let category = createTestCompanyCategory "IND" "MFG" "製造業"

            // Act
            do! CompanyCategoryRepository.insertAsync this.ConnectionString category

            // Assert
            let! found = CompanyCategoryRepository.findByIdAsync this.ConnectionString "IND" "MFG"
            found |> should not' (equal None)
            found.Value.CategoryName |> should equal "製造業"
        }

    [<Fact>]
    member this.``分類種別で取引先分類を検索できる``() =
        task {
            // Arrange
            let categoryType = createTestCategoryType "IND" "業種"
            do! CategoryTypeRepository.insertAsync this.ConnectionString categoryType

            let category1 = createTestCompanyCategory "IND" "MFG" "製造業"
            let category2 = createTestCompanyCategory "IND" "SVC" "サービス業"
            let category3 = createTestCompanyCategory "IND" "RTL" "小売業"

            do! CompanyCategoryRepository.insertAsync this.ConnectionString category1
            do! CompanyCategoryRepository.insertAsync this.ConnectionString category2
            do! CompanyCategoryRepository.insertAsync this.ConnectionString category3

            // Act
            let! categories = CompanyCategoryRepository.findByCategoryTypeAsync this.ConnectionString "IND"
            let categoryList = categories |> Seq.toList

            // Assert
            categoryList |> should haveLength 3
            categoryList |> List.forall (fun c -> c.CategoryTypeCode = "IND") |> should be True
        }

    [<Fact>]
    member this.``取引先分類を更新できる``() =
        task {
            // Arrange
            let categoryType = createTestCategoryType "IND" "業種"
            do! CategoryTypeRepository.insertAsync this.ConnectionString categoryType

            let category = createTestCompanyCategory "IND" "MFG" "製造業"
            do! CompanyCategoryRepository.insertAsync this.ConnectionString category

            // Act
            let updated =
                { category with
                    CategoryName = "製造業（更新）"
                    UpdatedAt = DateTime.Now
                    UpdatedBy = "updater" }
            do! CompanyCategoryRepository.updateAsync this.ConnectionString updated

            // Assert
            let! result = CompanyCategoryRepository.findByIdAsync this.ConnectionString "IND" "MFG"
            result |> should not' (equal None)
            result.Value.CategoryName |> should equal "製造業（更新）"
        }

    [<Fact>]
    member this.``取引先分類を削除できる``() =
        task {
            // Arrange
            let categoryType = createTestCategoryType "IND" "業種"
            do! CategoryTypeRepository.insertAsync this.ConnectionString categoryType

            let category = createTestCompanyCategory "IND" "MFG" "製造業"
            do! CompanyCategoryRepository.insertAsync this.ConnectionString category

            // Act
            do! CompanyCategoryRepository.deleteAsync this.ConnectionString "IND" "MFG"

            // Assert
            let! found = CompanyCategoryRepository.findByIdAsync this.ConnectionString "IND" "MFG"
            found |> should equal None
        }

    // ========================================
    // 取引先分類所属マスタのテスト（多対多関係）
    // ========================================

    [<Fact>]
    member this.``取引先に分類を割り当てられる``() =
        task {
            // Arrange
            do! setupCompany this.ConnectionString "COMP001"

            let categoryType = createTestCategoryType "IND" "業種"
            do! CategoryTypeRepository.insertAsync this.ConnectionString categoryType

            let category = createTestCompanyCategory "IND" "MFG" "製造業"
            do! CompanyCategoryRepository.insertAsync this.ConnectionString category

            let categoryGroup = createTestCategoryGroup "IND" "MFG" "COMP001"

            // Act
            do! CompanyCategoryGroupRepository.insertAsync this.ConnectionString categoryGroup

            // Assert
            let! found = CompanyCategoryGroupRepository.findByIdAsync this.ConnectionString "IND" "MFG" "COMP001"
            found |> should not' (equal None)
            found.Value.CompanyCode |> should equal "COMP001"
        }

    [<Fact>]
    member this.``取引先に複数の分類を割り当てられる``() =
        task {
            // Arrange
            do! setupCompany this.ConnectionString "COMP001"

            let categoryType = createTestCategoryType "IND" "業種"
            do! CategoryTypeRepository.insertAsync this.ConnectionString categoryType

            let category1 = createTestCompanyCategory "IND" "MFG" "製造業"
            let category2 = createTestCompanyCategory "IND" "SVC" "サービス業"

            do! CompanyCategoryRepository.insertAsync this.ConnectionString category1
            do! CompanyCategoryRepository.insertAsync this.ConnectionString category2

            let categoryGroup1 = createTestCategoryGroup "IND" "MFG" "COMP001"
            let categoryGroup2 = createTestCategoryGroup "IND" "SVC" "COMP001"

            // Act
            do! CompanyCategoryGroupRepository.insertAsync this.ConnectionString categoryGroup1
            do! CompanyCategoryGroupRepository.insertAsync this.ConnectionString categoryGroup2

            // Assert
            let! categories = CompanyCategoryGroupRepository.findByCompanyCodeAsync this.ConnectionString "COMP001"
            let categoryList = categories |> Seq.toList

            categoryList |> should haveLength 2
            categoryList |> List.exists (fun c -> c.CategoryCode = "MFG") |> should be True
            categoryList |> List.exists (fun c -> c.CategoryCode = "SVC") |> should be True
        }

    [<Fact>]
    member this.``分類に所属する取引先を検索できる``() =
        task {
            // Arrange: グループを一度だけ作成
            let group = {
                CompanyGroupCode = "GRP1"
                CompanyGroupName = "テストグループ"
                CreatedAt = DateTime.Now
                CreatedBy = "admin"
                UpdatedAt = DateTime.Now
                UpdatedBy = "admin"
            }
            do! CompanyGroupRepository.insertAsync this.ConnectionString group

            let company1 = {
                CompanyCode = "COMP001"
                CompanyName = "テスト商事1"
                CompanyNameKana = None
                SupplierType = 0
                ZipCode = None
                State = None
                Address1 = None
                Address2 = None
                NoSalesFlag = 0
                WideUseType = 0
                CompanyGroupCode = "GRP1"
                MaxCredit = 0
                TempCreditUp = 0
                CreatedAt = DateTime.Now
                CreatedBy = "admin"
                UpdatedAt = DateTime.Now
                UpdatedBy = "admin"
            }
            do! CompanyRepository.insertAsync this.ConnectionString company1

            let company2 = {
                CompanyCode = "COMP002"
                CompanyName = "テスト商事2"
                CompanyNameKana = None
                SupplierType = 0
                ZipCode = None
                State = None
                Address1 = None
                Address2 = None
                NoSalesFlag = 0
                WideUseType = 0
                CompanyGroupCode = "GRP1"
                MaxCredit = 0
                TempCreditUp = 0
                CreatedAt = DateTime.Now
                CreatedBy = "admin"
                UpdatedAt = DateTime.Now
                UpdatedBy = "admin"
            }
            do! CompanyRepository.insertAsync this.ConnectionString company2

            let categoryType = createTestCategoryType "IND" "業種"
            do! CategoryTypeRepository.insertAsync this.ConnectionString categoryType

            let category = createTestCompanyCategory "IND" "MFG" "製造業"
            do! CompanyCategoryRepository.insertAsync this.ConnectionString category

            let categoryGroup1 = createTestCategoryGroup "IND" "MFG" "COMP001"
            let categoryGroup2 = createTestCategoryGroup "IND" "MFG" "COMP002"

            do! CompanyCategoryGroupRepository.insertAsync this.ConnectionString categoryGroup1
            do! CompanyCategoryGroupRepository.insertAsync this.ConnectionString categoryGroup2

            // Act
            let! companies = CompanyCategoryGroupRepository.findByCategoryAsync this.ConnectionString "IND" "MFG"
            let companyList = companies |> Seq.toList

            // Assert
            companyList |> should haveLength 2
            companyList |> List.exists (fun c -> c.CompanyCode = "COMP001") |> should be True
            companyList |> List.exists (fun c -> c.CompanyCode = "COMP002") |> should be True
        }

    [<Fact>]
    member this.``取引先の分類所属を削除できる``() =
        task {
            // Arrange
            do! setupCompany this.ConnectionString "COMP001"

            let categoryType = createTestCategoryType "IND" "業種"
            do! CategoryTypeRepository.insertAsync this.ConnectionString categoryType

            let category = createTestCompanyCategory "IND" "MFG" "製造業"
            do! CompanyCategoryRepository.insertAsync this.ConnectionString category

            let categoryGroup = createTestCategoryGroup "IND" "MFG" "COMP001"
            do! CompanyCategoryGroupRepository.insertAsync this.ConnectionString categoryGroup

            // Act
            do! CompanyCategoryGroupRepository.deleteAsync this.ConnectionString "IND" "MFG" "COMP001"

            // Assert
            let! found = CompanyCategoryGroupRepository.findByIdAsync this.ConnectionString "IND" "MFG" "COMP001"
            found |> should equal None
        }
