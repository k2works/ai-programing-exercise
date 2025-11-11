module SalesManagement.Tests.IntegrationTests.CustomerTests

open System
open Xunit
open FsUnit.Xunit
open SalesManagement.Domain.Models
open SalesManagement.Infrastructure.Repositories
open SalesManagement.Tests.DatabaseTestBase

type CustomerTests() =
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

    let createTestCustomer code branch name =
        {
            CustomerCode = code
            CustomerBranch = branch
            CustomerType = 0
            ArCode = code
            ArBranch = Some branch
            PayerCode = code
            PayerBranch = Some branch
            CustomerName = name
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

    [<Fact>]
    member this.``同一取引先に複数の顧客を枝番で登録できる``() =
        task {
            // Arrange
            do! setupCompany this.ConnectionString "COMP001"
            do! setupEmployee this.ConnectionString "EMP001"

            let customer1 = createTestCustomer "COMP001" 1 "本社営業部"
            let customer2 = createTestCustomer "COMP001" 2 "大阪支社"

            // Act
            do! CustomerRepository.insertAsync this.ConnectionString customer1
            do! CustomerRepository.insertAsync this.ConnectionString customer2

            // Assert
            let! customers = CustomerRepository.findByCustomerCodeAsync this.ConnectionString "COMP001"
            let customerList = customers |> Seq.toList

            customerList |> should haveLength 2
            customerList.[0].CustomerBranch |> should equal 1
            customerList.[1].CustomerBranch |> should equal 2
        }

    [<Fact>]
    member this.``複合主キーで顧客を特定できる``() =
        task {
            // Arrange
            do! setupCompany this.ConnectionString "COMP001"
            do! setupEmployee this.ConnectionString "EMP001"

            let customer = createTestCustomer "COMP001" 1 "本社営業部"

            // Act
            do! CustomerRepository.insertAsync this.ConnectionString customer

            // Assert
            let! found = CustomerRepository.findByIdAsync this.ConnectionString "COMP001" 1
            found |> should not' (equal None)
            found.Value.CustomerName |> should equal "本社営業部"
            found.Value.CustomerBranch |> should equal 1
        }

    [<Fact>]
    member this.``取引先を顧客と仕入先の両方の役割で登録できる``() =
        task {
            // Arrange
            do! setupCompany this.ConnectionString "COMP001"
            do! setupEmployee this.ConnectionString "EMP001"

            // 顧客としての役割を登録
            let customer = createTestCustomer "COMP001" 1 "テスト商事 本社"
            do! CustomerRepository.insertAsync this.ConnectionString customer

            // 仕入先としての役割を登録
            let supplier = {
                SupplierCode = "COMP001"
                SupplierBranch = 1
                SupplierType = 0
                SupplierName = "テスト商事 仕入部"
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
            do! SupplierRepository.insertAsync this.ConnectionString supplier

            // Assert
            let! customers = CustomerRepository.findByCompanyCodeAsync this.ConnectionString "COMP001"
            let! suppliers = SupplierRepository.findByCompanyCodeAsync this.ConnectionString "COMP001"

            customers |> Seq.length |> should equal 1
            suppliers |> Seq.length |> should equal 1
        }
