module SalesManagement.Tests.IntegrationTests.SupplierTests

open System
open Xunit
open FsUnit.Xunit
open SalesManagement.Domain.Models
open SalesManagement.Infrastructure.Repositories
open SalesManagement.Tests.DatabaseTestBase

type SupplierTests() =
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

    let setupEmployee (connectionString: string) employeeCode =
        task {
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
            do! DepartmentRepository.insertAsync connectionString department

            let employee = {
                EmployeeCode = employeeCode
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
            do! EmployeeRepository.insertAsync connectionString employee
        }

    let createTestSupplier code branch name =
        {
            SupplierCode = code
            SupplierBranch = branch
            SupplierType = 0
            SupplierName = name
            SupplierNameKana = None
            EmployeeCode = "EMP001"
            SupplierCloseDate = 31
            SupplierPayMonths = 1
            SupplierPayDates = Some 31
            SupplierPayMethod = 1
            CreatedAt = DateTime.Now
            CreatedBy = "admin"
            UpdatedAt = DateTime.Now
            UpdatedBy = "admin"
        }

    [<Fact>]
    member this.``同一取引先に複数の仕入先を枝番で登録できる``() =
        task {
            // Arrange
            do! setupCompany this.ConnectionString "COMP001"
            do! setupEmployee this.ConnectionString "EMP001"

            let supplier1 = createTestSupplier "COMP001" 1 "本社仕入部"
            let supplier2 = createTestSupplier "COMP001" 2 "大阪支社仕入部"

            // Act
            do! SupplierRepository.insertAsync this.ConnectionString supplier1
            do! SupplierRepository.insertAsync this.ConnectionString supplier2

            // Assert
            let! suppliers = SupplierRepository.findByCompanyCodeAsync this.ConnectionString "COMP001"
            let supplierList = suppliers |> Seq.toList

            supplierList |> should haveLength 2
            supplierList.[0].SupplierBranch |> should equal 1
            supplierList.[1].SupplierBranch |> should equal 2
        }

    [<Fact>]
    member this.``複合主キーで仕入先を特定できる``() =
        task {
            // Arrange
            do! setupCompany this.ConnectionString "COMP001"
            do! setupEmployee this.ConnectionString "EMP001"

            let supplier = createTestSupplier "COMP001" 1 "本社仕入部"

            // Act
            do! SupplierRepository.insertAsync this.ConnectionString supplier

            // Assert
            let! found = SupplierRepository.findByIdAsync this.ConnectionString "COMP001" 1
            found |> should not' (equal None)
            found.Value.SupplierName |> should equal "本社仕入部"
            found.Value.SupplierBranch |> should equal 1
        }

    [<Fact>]
    member this.``仕入先を更新できる``() =
        task {
            // Arrange
            do! setupCompany this.ConnectionString "COMP001"
            do! setupEmployee this.ConnectionString "EMP001"

            let supplier = createTestSupplier "COMP001" 1 "本社仕入部"
            do! SupplierRepository.insertAsync this.ConnectionString supplier

            // Act
            let updated =
                { supplier with
                    SupplierName = "本社仕入部（変更後）"
                    SupplierPayMonths = 2
                    UpdatedAt = DateTime.Now
                    UpdatedBy = "updater" }
            do! SupplierRepository.updateAsync this.ConnectionString updated

            // Assert
            let! result = SupplierRepository.findByIdAsync this.ConnectionString "COMP001" 1
            result |> should not' (equal None)
            result.Value.SupplierName |> should equal "本社仕入部（変更後）"
            result.Value.SupplierPayMonths |> should equal 2
        }

    [<Fact>]
    member this.``仕入先を削除できる``() =
        task {
            // Arrange
            do! setupCompany this.ConnectionString "COMP001"
            do! setupEmployee this.ConnectionString "EMP001"

            let supplier = createTestSupplier "COMP001" 1 "本社仕入部"
            do! SupplierRepository.insertAsync this.ConnectionString supplier

            // Act
            do! SupplierRepository.deleteAsync this.ConnectionString "COMP001" 1

            // Assert
            let! found = SupplierRepository.findByIdAsync this.ConnectionString "COMP001" 1
            found |> should equal None
        }
