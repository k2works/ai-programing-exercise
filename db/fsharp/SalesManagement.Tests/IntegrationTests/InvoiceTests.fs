module SalesManagement.Tests.IntegrationTests.InvoiceTests

open System
open Xunit
open FsUnit.Xunit
open SalesManagement.Domain.Models
open SalesManagement.Infrastructure.Repositories
open SalesManagement.Tests.DatabaseTestBase

type InvoiceTests() =
    inherit DatabaseTestBase()

    let setupCompany (connectionString: string) (companyCode: string) =
        task {
            let groupCode = companyCode.Substring(0, min 4 companyCode.Length)
            let group = {
                CompanyGroupCode = groupCode
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
                CompanyGroupCode = groupCode
                MaxCredit = 0
                TempCreditUp = 0
                CreatedAt = DateTime.Now
                CreatedBy = "admin"
                UpdatedAt = DateTime.Now
                UpdatedBy = "admin"
            }
            do! CompanyRepository.insertAsync connectionString company
        }

    let setupEmployee (connectionString: string) (employeeCode: string) =
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

    let setupCustomer (connectionString: string) (customerCode: string) (customerBranch: int) (employeeCode: string) =
        task {
            let customer = {
                CustomerCode = customerCode
                CustomerBranch = customerBranch
                CustomerType = 0
                ArCode = customerCode
                ArBranch = Some customerBranch
                PayerCode = customerCode
                PayerBranch = Some customerBranch
                CustomerName = "テスト顧客"
                CustomerNameKana = None
                EmployeeCode = employeeCode
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
            do! CustomerRepository.insertAsync connectionString customer
        }

    let createTestInvoice invoiceNo customerCode customerBranch amount clearedAmount =
        {
            InvoiceNo = invoiceNo
            InvoiceDate = DateTime.Now
            CustomerCode = customerCode
            CustomerBranch = customerBranch
            SalesSlipNo = None
            InvoiceAmount = amount
            ClearedAmount = clearedAmount
            Remarks = None
            DepartmentCode = "10000"
            CreatedAt = DateTime.Now
            CreatedBy = "system"
            UpdatedAt = DateTime.Now
            UpdatedBy = "system"
        }

    [<Fact>]
    member this.``請求データを登録できる``() =
        task {
            // Arrange
            do! setupCompany this.ConnectionString "COMP001"
            do! setupEmployee this.ConnectionString "EMP001"
            do! setupCustomer this.ConnectionString "COMP001" 1 "EMP001"

            let invoice = createTestInvoice "INV001" "COMP001" 1 100000m 0m

            // Act
            do! InvoiceRepository.insertAsync this.ConnectionString invoice

            // Assert
            let! found = InvoiceRepository.findByIdAsync this.ConnectionString "INV001"
            found |> should not' (equal None)
            found.Value.InvoiceAmount |> should equal 100000m
            found.Value.ClearedAmount |> should equal 0m
            found.Value.CustomerCode |> should equal "COMP001"
        }

    [<Fact>]
    member this.``顧客別に請求を取得できる``() =
        task {
            // Arrange
            do! setupCompany this.ConnectionString "COMP001"
            do! setupEmployee this.ConnectionString "EMP001"
            do! setupCustomer this.ConnectionString "COMP001" 1 "EMP001"

            let invoice1 = createTestInvoice "INV001" "COMP001" 1 100000m 0m
            let invoice2 = createTestInvoice "INV002" "COMP001" 1 50000m 0m

            do! InvoiceRepository.insertAsync this.ConnectionString invoice1
            do! InvoiceRepository.insertAsync this.ConnectionString invoice2

            // Act
            let! invoices = InvoiceRepository.findByCustomerAsync this.ConnectionString "COMP001" 1

            // Assert
            let invoiceList = invoices |> Seq.toList
            invoiceList |> should haveLength 2
        }

    [<Fact>]
    member this.``未消込の請求を取得できる``() =
        task {
            // Arrange
            do! setupCompany this.ConnectionString "COMP001"
            do! setupEmployee this.ConnectionString "EMP001"
            do! setupCustomer this.ConnectionString "COMP001" 1 "EMP001"

            let invoice1 = createTestInvoice "INV001" "COMP001" 1 100000m 0m       // 未消込
            let invoice2 = createTestInvoice "INV002" "COMP001" 1 50000m 30000m    // 一部消込
            let invoice3 = createTestInvoice "INV003" "COMP001" 1 80000m 80000m    // 完全消込

            do! InvoiceRepository.insertAsync this.ConnectionString invoice1
            do! InvoiceRepository.insertAsync this.ConnectionString invoice2
            do! InvoiceRepository.insertAsync this.ConnectionString invoice3

            // Act
            let! unclearedInvoices = InvoiceRepository.findUnclearedAsync this.ConnectionString "COMP001" 1

            // Assert
            let unclearedList = unclearedInvoices |> Seq.toList
            unclearedList |> should haveLength 2
            unclearedList |> List.exists (fun i -> i.InvoiceNo = "INV001") |> should be True
            unclearedList |> List.exists (fun i -> i.InvoiceNo = "INV002") |> should be True
            unclearedList |> List.exists (fun i -> i.InvoiceNo = "INV003") |> should be False
        }

    [<Fact>]
    member this.``請求消込金額を更新できる``() =
        task {
            // Arrange
            do! setupCompany this.ConnectionString "COMP001"
            do! setupEmployee this.ConnectionString "EMP001"
            do! setupCustomer this.ConnectionString "COMP001" 1 "EMP001"

            let invoice = createTestInvoice "INV001" "COMP001" 1 100000m 0m
            do! InvoiceRepository.insertAsync this.ConnectionString invoice

            // Act: 最初の消込
            do! InvoiceRepository.updateClearedAmountAsync this.ConnectionString "INV001" 30000m

            // Assert
            let! found1 = InvoiceRepository.findByIdAsync this.ConnectionString "INV001"
            found1.Value.ClearedAmount |> should equal 30000m

            // Act: さらに消込
            do! InvoiceRepository.updateClearedAmountAsync this.ConnectionString "INV001" 20000m

            // Assert
            let! found2 = InvoiceRepository.findByIdAsync this.ConnectionString "INV001"
            found2.Value.ClearedAmount |> should equal 50000m
        }

    [<Fact>]
    member this.``請求を削除できる``() =
        task {
            // Arrange
            do! setupCompany this.ConnectionString "COMP001"
            do! setupEmployee this.ConnectionString "EMP001"
            do! setupCustomer this.ConnectionString "COMP001" 1 "EMP001"

            let invoice = createTestInvoice "INV001" "COMP001" 1 100000m 0m
            do! InvoiceRepository.insertAsync this.ConnectionString invoice

            // Act
            do! InvoiceRepository.deleteAsync this.ConnectionString "INV001"

            // Assert
            let! deleted = InvoiceRepository.findByIdAsync this.ConnectionString "INV001"
            deleted |> should equal None
        }
