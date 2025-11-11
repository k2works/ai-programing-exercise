module SalesManagement.Tests.IntegrationTests.CompanyTests

open System
open Xunit
open FsUnit.Xunit
open SalesManagement.Domain.Models
open SalesManagement.Infrastructure.Repositories
open SalesManagement.Tests.DatabaseTestBase

type CompanyTests() =
    inherit DatabaseTestBase()

    let createTestGroup code name =
        {
            CompanyGroupCode = code
            CompanyGroupName = name
            CreatedAt = DateTime.Now
            CreatedBy = "admin"
            UpdatedAt = DateTime.Now
            UpdatedBy = "admin"
        }

    let createTestCompany code name groupCode =
        {
            CompanyCode = code
            CompanyName = name
            CompanyNameKana = Some "テストショウジ"
            SupplierType = 0
            ZipCode = Some "1000001"
            State = Some "東京都"
            Address1 = Some "千代田区"
            Address2 = None
            NoSalesFlag = 0
            WideUseType = 0
            CompanyGroupCode = groupCode
            MaxCredit = 10000000
            TempCreditUp = 0
            CreatedAt = DateTime.Now
            CreatedBy = "admin"
            UpdatedAt = DateTime.Now
            UpdatedBy = "admin"
        }

    [<Fact>]
    member this.``取引先を登録できる``() =
        task {
            // Arrange
            let group = createTestGroup "GRP1" "大手企業"
            do! CompanyGroupRepository.insertAsync this.ConnectionString group

            let company = createTestCompany "COMP001" "株式会社テスト商事" "GRP1"

            // Act
            do! CompanyRepository.insertAsync this.ConnectionString company

            // Assert
            let! found = CompanyRepository.findByIdAsync this.ConnectionString "COMP001"
            found |> should not' (equal None)
            found.Value.CompanyName |> should equal "株式会社テスト商事"
            found.Value.CompanyGroupCode |> should equal "GRP1"
        }

    [<Fact>]
    member this.``取引先グループでまとめて取得できる``() =
        task {
            // Arrange
            let group = createTestGroup "GRP1" "大手企業"
            do! CompanyGroupRepository.insertAsync this.ConnectionString group

            do! CompanyRepository.insertAsync this.ConnectionString (createTestCompany "COMP001" "A商事" "GRP1")
            do! CompanyRepository.insertAsync this.ConnectionString (createTestCompany "COMP002" "B商事" "GRP1")
            do! CompanyRepository.insertAsync this.ConnectionString (createTestCompany "COMP003" "C商事" "GRP1")

            // Act
            let! companies = CompanyRepository.findByGroupCodeAsync this.ConnectionString "GRP1"
            let companyList = companies |> Seq.toList

            // Assert
            companyList |> should haveLength 3
            companyList |> List.forall (fun c -> c.CompanyGroupCode = "GRP1") |> should be True
        }

    [<Fact>]
    member this.``取引先を更新できる``() =
        task {
            // Arrange
            let group = createTestGroup "GRP1" "大手企業"
            do! CompanyGroupRepository.insertAsync this.ConnectionString group

            let company = createTestCompany "COMP001" "株式会社テスト商事" "GRP1"
            do! CompanyRepository.insertAsync this.ConnectionString company

            // Act
            let updated =
                { company with
                    CompanyName = "株式会社テスト商事（変更後）"
                    MaxCredit = 20000000
                    UpdatedAt = DateTime.Now
                    UpdatedBy = "updater" }
            do! CompanyRepository.updateAsync this.ConnectionString updated

            // Assert
            let! result = CompanyRepository.findByIdAsync this.ConnectionString "COMP001"
            result |> should not' (equal None)
            result.Value.CompanyName |> should equal "株式会社テスト商事（変更後）"
            result.Value.MaxCredit |> should equal 20000000
        }
