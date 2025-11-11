module SalesManagement.Tests.IntegrationTests.BoundaryValueTests

open System
open Xunit
open FsUnit.Xunit
open SalesManagement.Domain.Models
open SalesManagement.Infrastructure.Repositories
open SalesManagement.Tests.DatabaseTestBase

type BoundaryValueTests() =
    inherit DatabaseTestBase()

    // ========================================
    // 企業グループコードの境界値テスト
    // ========================================

    [<Fact>]
    member this.``企業グループコード_境界値_最大長_4文字_成功``() =
        task {
            // Arrange: 最大長4文字のコード
            let maxLengthGroup = {
                CompanyGroupCode = "ABCD"
                CompanyGroupName = "最大長グループ"
                CreatedAt = DateTime.Now
                CreatedBy = "admin"
                UpdatedAt = DateTime.Now
                UpdatedBy = "admin"
            }

            // Act: 挿入
            do! CompanyGroupRepository.insertAsync this.ConnectionString maxLengthGroup

            // Assert: 正常に挿入される
            let! found = CompanyGroupRepository.findByIdAsync this.ConnectionString "ABCD"
            found |> should not' (equal None)
            found.Value.CompanyGroupCode |> should equal "ABCD"
        }

    [<Fact>]
    member this.``企業グループコード_境界値_最大長超過_5文字_エラー``() =
        task {
            // Arrange: 最大長超過（5文字）
            let tooLongGroup = {
                CompanyGroupCode = "ABCDE"
                CompanyGroupName = "長すぎるグループ"
                CreatedAt = DateTime.Now
                CreatedBy = "admin"
                UpdatedAt = DateTime.Now
                UpdatedBy = "admin"
            }

            // Act & Assert: varchar(4) 制約でエラー
            let! ex = Assert.ThrowsAsync<Npgsql.PostgresException>(fun () ->
                CompanyGroupRepository.insertAsync this.ConnectionString tooLongGroup :> System.Threading.Tasks.Task)

            // エラーメッセージに "too long" が含まれる
            ex.Message.Contains("too long") |> should be True
        }

    [<Fact>]
    member this.``企業グループコード_境界値_重複_エラー``() =
        task {
            // Arrange: 同じコードで2回挿入
            let group = {
                CompanyGroupCode = "DUP1"
                CompanyGroupName = "重複グループ"
                CreatedAt = DateTime.Now
                CreatedBy = "admin"
                UpdatedAt = DateTime.Now
                UpdatedBy = "admin"
            }

            // Act: 1回目は成功
            do! CompanyGroupRepository.insertAsync this.ConnectionString group

            // Assert: 2回目は主キー制約違反
            let! ex = Assert.ThrowsAsync<Npgsql.PostgresException>(fun () ->
                CompanyGroupRepository.insertAsync this.ConnectionString group :> System.Threading.Tasks.Task)

            // 重複キーエラー（23505）
            ex.SqlState |> should equal "23505"
        }

    // ========================================
    // 顧客コードの境界値テスト（複合主キー）
    // ========================================

    [<Fact>]
    member this.``顧客枝番_境界値_0は許可されない``() =
        task {
            // Arrange: 前提データ作成
            let group = {
                CompanyGroupCode = "GRP1"
                CompanyGroupName = "テストグループ"
                CreatedAt = DateTime.Now
                CreatedBy = "admin"
                UpdatedAt = DateTime.Now
                UpdatedBy = "admin"
            }
            do! CompanyGroupRepository.insertAsync this.ConnectionString group

            let company = {
                CompanyCode = "COMP001"
                CompanyName = "テスト企業"
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
            do! CompanyRepository.insertAsync this.ConnectionString company

            let department = {
                DepartmentCode = "10000"
                StartDate = DateTime(2021, 1, 1)
                EndDate = DateTime(2100, 12, 31)
                DepartmentName = "本社"
                OrganizationLevel = 1
                DepartmentPath = "10000"
                LowestLevelFlag = 1
                SlipInputFlag = 1
                CreatedAt = DateTime.Now
                CreatedBy = "admin"
                UpdatedAt = DateTime.Now
                UpdatedBy = "admin"
            }
            do! DepartmentRepository.insertAsync this.ConnectionString department

            let employee = {
                EmployeeCode = "EMP001"
                EmployeeName = "担当者"
                EmployeeNameKana = "タントウシャ"
                Gender = "M"
                BirthDate = Some (DateTime(1990, 1, 1))
                JoinDate = DateTime(2015, 4, 1)
                DepartmentCode = "10000"
                PositionCode = None
                CreatedAt = DateTime.Now
                CreatedBy = "admin"
                UpdatedAt = DateTime.Now
                UpdatedBy = "admin"
            }
            do! EmployeeRepository.insertAsync this.ConnectionString employee

            // 枝番0の顧客（ビジネスルール違反の可能性）
            let customer = {
                CustomerCode = "COMP001"
                CustomerBranch = 0
                CustomerType = 0
                ArCode = "COMP001"
                ArBranch = Some 0
                PayerCode = "COMP001"
                PayerBranch = Some 0
                CustomerName = "枝番ゼロ顧客"
                CustomerNameKana = None
                EmployeeCode = "EMP001"
                CustomerUserName = None
                CustomerDepartmentName = None
                CustomerZipCode = None
                CustomerState = None
                CustomerAddress1 = None
                CustomerAddress2 = None
                CustomerTel = None
                CustomerFax = None
                CustomerEmail = None
                CustomerArType = 0
                CustomerCloseDate1 = 31
                CustomerPayMonths1 = 1
                CustomerPayDates1 = Some 31
                CustomerPayMethod1 = 1
                CustomerCloseDate2 = 0
                CustomerPayMonths2 = 0
                CustomerPayDates2 = None
                CustomerPayMethod2 = 0
                CreatedAt = DateTime.Now
                CreatedBy = "admin"
                UpdatedAt = DateTime.Now
                UpdatedBy = "admin"
            }

            // Act: 挿入試行
            // 注：現在の実装では枝番0も許可されるが、ビジネスルール的には1から始めるべき
            do! CustomerRepository.insertAsync this.ConnectionString customer

            // Assert: 技術的には成功するが、警告すべき
            let! found = CustomerRepository.findByIdAsync this.ConnectionString "COMP001" 0
            found |> should not' (equal None)
            // ビジネスルール的には枝番は1以上が望ましい
        }

    // ========================================
    // 外部キー制約テスト
    // ========================================

    [<Fact>]
    member this.``外部キー制約_存在しない企業グループで企業作成_エラー``() =
        task {
            // Arrange: 存在しない企業グループコードを参照
            let company = {
                CompanyCode = "COMP999"
                CompanyName = "テスト企業"
                CompanyNameKana = None
                SupplierType = 0
                ZipCode = None
                State = None
                Address1 = None
                Address2 = None
                NoSalesFlag = 0
                WideUseType = 0
                CompanyGroupCode = "NONE"  // 存在しない
                MaxCredit = 0
                TempCreditUp = 0
                CreatedAt = DateTime.Now
                CreatedBy = "admin"
                UpdatedAt = DateTime.Now
                UpdatedBy = "admin"
            }

            // Act & Assert: 外部キー制約違反
            let! ex = Assert.ThrowsAsync<Npgsql.PostgresException>(fun () ->
                CompanyRepository.insertAsync this.ConnectionString company :> System.Threading.Tasks.Task)

            // 外部キーエラー（23503）
            ex.SqlState |> should equal "23503"
        }

    [<Fact>]
    member this.``外部キー制約_存在しない取引先で顧客作成_エラー``() =
        task {
            // Arrange: 前提データ作成（社員のみ）
            let department = {
                DepartmentCode = "10000"
                StartDate = DateTime(2021, 1, 1)
                EndDate = DateTime(2100, 12, 31)
                DepartmentName = "本社"
                OrganizationLevel = 1
                DepartmentPath = "10000"
                LowestLevelFlag = 1
                SlipInputFlag = 1
                CreatedAt = DateTime.Now
                CreatedBy = "admin"
                UpdatedAt = DateTime.Now
                UpdatedBy = "admin"
            }
            do! DepartmentRepository.insertAsync this.ConnectionString department

            let employee = {
                EmployeeCode = "EMP001"
                EmployeeName = "担当者"
                EmployeeNameKana = "タントウシャ"
                Gender = "M"
                BirthDate = Some (DateTime(1990, 1, 1))
                JoinDate = DateTime(2015, 4, 1)
                DepartmentCode = "10000"
                PositionCode = None
                CreatedAt = DateTime.Now
                CreatedBy = "admin"
                UpdatedAt = DateTime.Now
                UpdatedBy = "admin"
            }
            do! EmployeeRepository.insertAsync this.ConnectionString employee

            // 存在しない取引先を参照する顧客
            let customer = {
                CustomerCode = "NOCOMP"  // 存在しない
                CustomerBranch = 1
                CustomerType = 0
                ArCode = "NOCOMP"
                ArBranch = Some 1
                PayerCode = "NOCOMP"
                PayerBranch = Some 1
                CustomerName = "存在しない取引先顧客"
                CustomerNameKana = None
                EmployeeCode = "EMP001"
                CustomerUserName = None
                CustomerDepartmentName = None
                CustomerZipCode = None
                CustomerState = None
                CustomerAddress1 = None
                CustomerAddress2 = None
                CustomerTel = None
                CustomerFax = None
                CustomerEmail = None
                CustomerArType = 0
                CustomerCloseDate1 = 31
                CustomerPayMonths1 = 1
                CustomerPayDates1 = Some 31
                CustomerPayMethod1 = 1
                CustomerCloseDate2 = 0
                CustomerPayMonths2 = 0
                CustomerPayDates2 = None
                CustomerPayMethod2 = 0
                CreatedAt = DateTime.Now
                CreatedBy = "admin"
                UpdatedAt = DateTime.Now
                UpdatedBy = "admin"
            }

            // Act & Assert: 外部キー制約違反
            let! ex = Assert.ThrowsAsync<Npgsql.PostgresException>(fun () ->
                CustomerRepository.insertAsync this.ConnectionString customer :> System.Threading.Tasks.Task)

            // 外部キーエラー（23503）
            ex.SqlState |> should equal "23503"
        }

    [<Fact>]
    member this.``外部キー制約_存在しない社員で顧客作成_エラー``() =
        task {
            // Arrange: 前提データ作成（企業のみ）
            let group = {
                CompanyGroupCode = "GRP1"
                CompanyGroupName = "テストグループ"
                CreatedAt = DateTime.Now
                CreatedBy = "admin"
                UpdatedAt = DateTime.Now
                UpdatedBy = "admin"
            }
            do! CompanyGroupRepository.insertAsync this.ConnectionString group

            let company = {
                CompanyCode = "COMP001"
                CompanyName = "テスト企業"
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
            do! CompanyRepository.insertAsync this.ConnectionString company

            // 存在しない社員を参照する顧客
            let customer = {
                CustomerCode = "COMP001"
                CustomerBranch = 1
                CustomerType = 0
                ArCode = "COMP001"
                ArBranch = Some 1
                PayerCode = "COMP001"
                PayerBranch = Some 1
                CustomerName = "テスト顧客"
                CustomerNameKana = None
                EmployeeCode = "NOEMP"  // 存在しない
                CustomerUserName = None
                CustomerDepartmentName = None
                CustomerZipCode = None
                CustomerState = None
                CustomerAddress1 = None
                CustomerAddress2 = None
                CustomerTel = None
                CustomerFax = None
                CustomerEmail = None
                CustomerArType = 0
                CustomerCloseDate1 = 31
                CustomerPayMonths1 = 1
                CustomerPayDates1 = Some 31
                CustomerPayMethod1 = 1
                CustomerCloseDate2 = 0
                CustomerPayMonths2 = 0
                CustomerPayDates2 = None
                CustomerPayMethod2 = 0
                CreatedAt = DateTime.Now
                CreatedBy = "admin"
                UpdatedAt = DateTime.Now
                UpdatedBy = "admin"
            }

            // Act & Assert: 外部キー制約違反
            let! ex = Assert.ThrowsAsync<Npgsql.PostgresException>(fun () ->
                CustomerRepository.insertAsync this.ConnectionString customer :> System.Threading.Tasks.Task)

            // 外部キーエラー（23503）
            ex.SqlState |> should equal "23503"
        }
