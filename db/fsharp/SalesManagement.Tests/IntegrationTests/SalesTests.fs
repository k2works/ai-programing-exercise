module SalesManagement.Tests.IntegrationTests.SalesTests

open System
open Xunit
open FsUnit.Xunit
open SalesManagement.Domain.Models
open SalesManagement.Infrastructure.Repositories
open SalesManagement.Tests.DatabaseTestBase

type SalesTests() =
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

    let setupCustomer (connectionString: string) customerCode customerBranch employeeCode =
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

    let setupOrder (connectionString: string) orderNo customerCode customerBranch employeeCode deptCode =
        task {
            let order = {
                OrderNo = orderNo
                OrderDate = DateTime.Now
                CustomerCode = customerCode
                CustomerBranch = customerBranch
                EmployeeCode = employeeCode
                DueDate = Some(DateTime.Now.AddDays(30.0))
                OrderAmount = 10000
                ConsumptionTax = 1000
                SlipComment = None
                DepartmentCode = deptCode
                CreatedAt = DateTime.Now
                CreatedBy = "system"
                UpdatedAt = DateTime.Now
                UpdatedBy = "system"
            }
            do! OrderRepository.insertAsync connectionString order
        }

    let createTestSales salesSlipNo orderNo customerCode customerBranch employeeCode deptCode amount =
        {
            SalesSlipNo = salesSlipNo
            SalesDate = DateTime.Now
            OrderNo = orderNo
            CustomerCode = customerCode
            CustomerBranch = customerBranch
            EmployeeCode = employeeCode
            SalesAmount = amount
            ConsumptionTax = amount / 10
            SlipComment = None
            DepartmentCode = deptCode
            CreatedAt = DateTime.Now
            CreatedBy = "system"
            UpdatedAt = DateTime.Now
            UpdatedBy = "system"
        }

    [<Fact>]
    member this.``売上を登録できる``() =
        task {
            // Arrange
            do! setupCompany this.ConnectionString "COMP001"
            do! setupEmployee this.ConnectionString "EMP001"
            do! setupCustomer this.ConnectionString "COMP001" 1 "EMP001"

            let sales = createTestSales "SAL001" None "COMP001" 1 "EMP001" "10000" 10000

            // Act
            do! SalesRepository.insertAsync this.ConnectionString sales

            // Assert
            let! found = SalesRepository.findByIdAsync this.ConnectionString "SAL001"
            found |> should not' (equal None)
            found.Value.SalesSlipNo |> should equal "SAL001"
            found.Value.SalesAmount |> should equal 10000
        }

    [<Fact>]
    member this.``受注番号を紐づけて売上を登録できる``() =
        task {
            // Arrange
            do! setupCompany this.ConnectionString "COMP001"
            do! setupEmployee this.ConnectionString "EMP001"
            do! setupCustomer this.ConnectionString "COMP001" 1 "EMP001"
            do! setupOrder this.ConnectionString "ORD001" "COMP001" 1 "EMP001" "10000"

            let sales = createTestSales "SAL001" (Some "ORD001") "COMP001" 1 "EMP001" "10000" 10000

            // Act
            do! SalesRepository.insertAsync this.ConnectionString sales

            // Assert
            let! found = SalesRepository.findByIdAsync this.ConnectionString "SAL001"
            found.Value.OrderNo |> should equal (Some "ORD001")
        }

    [<Fact>]
    member this.``受注番号で売上を検索できる``() =
        task {
            // Arrange
            do! setupCompany this.ConnectionString "COMP001"
            do! setupEmployee this.ConnectionString "EMP001"
            do! setupCustomer this.ConnectionString "COMP001" 1 "EMP001"
            do! setupOrder this.ConnectionString "ORD001" "COMP001" 1 "EMP001" "10000"

            let sales1 = createTestSales "SAL001" (Some "ORD001") "COMP001" 1 "EMP001" "10000" 10000
            let sales2 = createTestSales "SAL002" (Some "ORD001") "COMP001" 1 "EMP001" "10000" 5000

            do! SalesRepository.insertAsync this.ConnectionString sales1
            do! SalesRepository.insertAsync this.ConnectionString sales2

            // Act
            let! sales = SalesRepository.findByOrderNoAsync this.ConnectionString "ORD001"

            // Assert
            let salesList = sales |> Seq.toList
            salesList |> should haveLength 2
            salesList |> List.sumBy (fun s -> s.SalesAmount) |> should equal 15000
        }

    [<Fact>]
    member this.``顧客の売上を検索できる``() =
        task {
            // Arrange
            do! setupCompany this.ConnectionString "COMP001"
            do! setupEmployee this.ConnectionString "EMP001"
            do! setupCustomer this.ConnectionString "COMP001" 1 "EMP001"

            let sales1 = createTestSales "SAL001" None "COMP001" 1 "EMP001" "10000" 10000
            let sales2 = createTestSales "SAL002" None "COMP001" 1 "EMP001" "10000" 20000

            do! SalesRepository.insertAsync this.ConnectionString sales1
            do! SalesRepository.insertAsync this.ConnectionString sales2

            // Act
            let! sales = SalesRepository.findByCustomerAsync this.ConnectionString "COMP001" 1

            // Assert
            let salesList = sales |> Seq.toList
            salesList |> should haveLength 2
            salesList |> List.sumBy (fun s -> s.SalesAmount) |> should equal 30000
        }

    [<Fact>]
    member this.``売上を削除できる``() =
        task {
            // Arrange
            do! setupCompany this.ConnectionString "COMP001"
            do! setupEmployee this.ConnectionString "EMP001"
            do! setupCustomer this.ConnectionString "COMP001" 1 "EMP001"

            let sales = createTestSales "SAL001" None "COMP001" 1 "EMP001" "10000" 10000
            do! SalesRepository.insertAsync this.ConnectionString sales

            // Act
            do! SalesRepository.deleteAsync this.ConnectionString "SAL001"

            // Assert
            let! deleted = SalesRepository.findByIdAsync this.ConnectionString "SAL001"
            deleted |> should equal None
        }
