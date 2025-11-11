module SalesManagement.Tests.IntegrationTests.OrderTests

open System
open Xunit
open FsUnit.Xunit
open SalesManagement.Domain.Models
open SalesManagement.Infrastructure.Repositories
open SalesManagement.Tests.DatabaseTestBase

type OrderTests() =
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

    let createTestOrder orderNo customerCode customerBranch employeeCode deptCode amount =
        {
            OrderNo = orderNo
            OrderDate = DateTime.Now
            CustomerCode = customerCode
            CustomerBranch = customerBranch
            EmployeeCode = employeeCode
            DueDate = Some(DateTime.Now.AddDays(30.0))
            OrderAmount = amount
            ConsumptionTax = amount / 10
            SlipComment = None
            DepartmentCode = deptCode
            CreatedAt = DateTime.Now
            CreatedBy = "system"
            UpdatedAt = DateTime.Now
            UpdatedBy = "system"
        }

    [<Fact>]
    member this.``受注を登録できる``() =
        task {
            // Arrange
            do! setupCompany this.ConnectionString "COMP001"
            do! setupEmployee this.ConnectionString "EMP001"
            do! setupCustomer this.ConnectionString "COMP001" 1 "EMP001"

            let order = createTestOrder "ORD001" "COMP001" 1 "EMP001" "10000" 10000

            // Act
            do! OrderRepository.insertAsync this.ConnectionString order

            // Assert
            let! found = OrderRepository.findByIdAsync this.ConnectionString "ORD001"
            found |> should not' (equal None)
            found.Value.OrderNo |> should equal "ORD001"
            found.Value.OrderAmount |> should equal 10000
        }

    [<Fact>]
    member this.``顧客の受注を検索できる``() =
        task {
            // Arrange
            do! setupCompany this.ConnectionString "COMP001"
            do! setupEmployee this.ConnectionString "EMP001"
            do! setupCustomer this.ConnectionString "COMP001" 1 "EMP001"

            let order1 = createTestOrder "ORD001" "COMP001" 1 "EMP001" "10000" 10000
            let order2 = createTestOrder "ORD002" "COMP001" 1 "EMP001" "10000" 20000

            do! OrderRepository.insertAsync this.ConnectionString order1
            do! OrderRepository.insertAsync this.ConnectionString order2

            // Act
            let! orders = OrderRepository.findByCustomerAsync this.ConnectionString "COMP001" 1

            // Assert
            let orderList = orders |> Seq.toList
            orderList |> should haveLength 2
            orderList |> List.sumBy (fun o -> o.OrderAmount) |> should equal 30000
        }

    [<Fact>]
    member this.``受注を更新できる``() =
        task {
            // Arrange
            do! setupCompany this.ConnectionString "COMP001"
            do! setupEmployee this.ConnectionString "EMP001"
            do! setupCustomer this.ConnectionString "COMP001" 1 "EMP001"

            let order = createTestOrder "ORD001" "COMP001" 1 "EMP001" "10000" 10000
            do! OrderRepository.insertAsync this.ConnectionString order

            // Act
            let updated = { order with OrderAmount = 20000; ConsumptionTax = 2000 }
            do! OrderRepository.updateAsync this.ConnectionString updated

            // Assert
            let! found = OrderRepository.findByIdAsync this.ConnectionString "ORD001"
            found.Value.OrderAmount |> should equal 20000
            found.Value.ConsumptionTax |> should equal 2000
        }

    [<Fact>]
    member this.``受注を削除できる``() =
        task {
            // Arrange
            do! setupCompany this.ConnectionString "COMP001"
            do! setupEmployee this.ConnectionString "EMP001"
            do! setupCustomer this.ConnectionString "COMP001" 1 "EMP001"

            let order = createTestOrder "ORD001" "COMP001" 1 "EMP001" "10000" 10000
            do! OrderRepository.insertAsync this.ConnectionString order

            // Act
            do! OrderRepository.deleteAsync this.ConnectionString "ORD001"

            // Assert
            let! deleted = OrderRepository.findByIdAsync this.ConnectionString "ORD001"
            deleted |> should equal None
        }

    [<Fact>]
    member this.``全ての受注を取得できる``() =
        task {
            // Arrange
            do! setupCompany this.ConnectionString "COMP001"
            do! setupEmployee this.ConnectionString "EMP001"
            do! setupCustomer this.ConnectionString "COMP001" 1 "EMP001"

            let order1 = createTestOrder "ORD001" "COMP001" 1 "EMP001" "10000" 10000
            let order2 = createTestOrder "ORD002" "COMP001" 1 "EMP001" "10000" 20000

            do! OrderRepository.insertAsync this.ConnectionString order1
            do! OrderRepository.insertAsync this.ConnectionString order2

            // Act
            let! orders = OrderRepository.findAllAsync this.ConnectionString

            // Assert
            let orderList = orders |> Seq.toList
            orderList |> List.length |> should be (greaterThanOrEqualTo 2)
        }
