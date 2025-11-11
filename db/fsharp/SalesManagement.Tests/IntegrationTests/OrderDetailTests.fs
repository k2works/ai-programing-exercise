module SalesManagement.Tests.IntegrationTests.OrderDetailTests

open System
open Xunit
open FsUnit.Xunit
open SalesManagement.Domain.Models
open SalesManagement.Infrastructure.Repositories
open SalesManagement.Tests.DatabaseTestBase

type OrderDetailTests() =
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

    let setupProduct (connectionString: string) productCode =
        task {
            let category = {
                ProductCategoryCode = "CAT001"
                ProductCategoryName = "テストカテゴリ"
                ProductCategoryLevel = 1
                ProductCategoryPath = "CAT001"
                LowestLevelFlag = 1
                CreatedAt = DateTime.Now
                CreatedBy = "admin"
                UpdatedAt = DateTime.Now
                UpdatedBy = "admin"
            }
            // 商品分類が既に存在する場合はスキップ
            try
                do! ProductCategoryRepository.insertAsync connectionString category
            with
            | _ -> ()

            let product = {
                ProductCode = productCode
                ProductFormalName = "テスト商品"
                ProductAbbreviation = "テスト"
                ProductNameKana = "テストショウヒン"
                ProductType = "01"
                ModelNumber = None
                SellingPrice = 1000
                PurchasePrice = 800
                CostOfSales = 800
                TaxType = 1
                ProductCategoryCode = "CAT001"
                MiscellaneousType = 0
                InventoryManagementFlag = 1
                InventoryAllocationFlag = 1
                SupplierCode = None
                SupplierBranch = None
                CreatedAt = DateTime.Now
                CreatedBy = "admin"
                UpdatedAt = DateTime.Now
                UpdatedBy = "admin"
            }
            do! ProductRepository.insertAsync connectionString product
        }

    let createTestDetail orderNo lineNo productCode productName quantity unitPrice =
        {
            OrderNo = orderNo
            OrderLineNo = lineNo
            ProductCode = productCode
            ProductName = productName
            Quantity = quantity
            AllocatedQuantity = 0
            ShippingInstructionQuantity = 0
            ShippedQuantity = 0
            UnitPrice = unitPrice
            Amount = quantity * unitPrice
            ConsumptionTax = (quantity * unitPrice) / 10
            IsCompleted = 0
            CreatedAt = DateTime.Now
            CreatedBy = "system"
            UpdatedAt = DateTime.Now
            UpdatedBy = "system"
        }

    [<Fact>]
    member this.``受注明細を登録できる``() =
        task {
            // Arrange
            do! setupCompany this.ConnectionString "COMP001"
            do! setupEmployee this.ConnectionString "EMP001"
            do! setupCustomer this.ConnectionString "COMP001" 1 "EMP001"
            do! setupOrder this.ConnectionString "ORD001" "COMP001" 1 "EMP001" "10000"
            do! setupProduct this.ConnectionString "PROD001"

            let detail = createTestDetail "ORD001" 1 "PROD001" "商品A" 10 1000

            // Act
            do! OrderDetailRepository.insertAsync this.ConnectionString detail

            // Assert
            let! details = OrderDetailRepository.findByOrderNoAsync this.ConnectionString "ORD001"
            let detailList = details |> Seq.toList
            detailList |> should haveLength 1
            detailList.[0].ProductCode |> should equal "PROD001"
            detailList.[0].Quantity |> should equal 10
        }

    [<Fact>]
    member this.``複数の明細を登録できる``() =
        task {
            // Arrange
            do! setupCompany this.ConnectionString "COMP001"
            do! setupEmployee this.ConnectionString "EMP001"
            do! setupCustomer this.ConnectionString "COMP001" 1 "EMP001"
            do! setupOrder this.ConnectionString "ORD001" "COMP001" 1 "EMP001" "10000"
            do! setupProduct this.ConnectionString "PROD001"
            do! setupProduct this.ConnectionString "PROD002"

            let detail1 = createTestDetail "ORD001" 1 "PROD001" "商品A" 10 1000
            let detail2 = createTestDetail "ORD001" 2 "PROD002" "商品B" 5 2000

            do! OrderDetailRepository.insertAsync this.ConnectionString detail1
            do! OrderDetailRepository.insertAsync this.ConnectionString detail2

            // Act
            let! details = OrderDetailRepository.findByOrderNoAsync this.ConnectionString "ORD001"

            // Assert
            let detailList = details |> Seq.toList
            detailList |> should haveLength 2
            detailList |> List.sumBy (fun d -> d.Amount) |> should equal 20000
        }

    [<Fact>]
    member this.``出荷済数量を更新できる``() =
        task {
            // Arrange
            do! setupCompany this.ConnectionString "COMP001"
            do! setupEmployee this.ConnectionString "EMP001"
            do! setupCustomer this.ConnectionString "COMP001" 1 "EMP001"
            do! setupOrder this.ConnectionString "ORD001" "COMP001" 1 "EMP001" "10000"
            do! setupProduct this.ConnectionString "PROD001"

            let detail = createTestDetail "ORD001" 1 "PROD001" "商品A" 10 1000
            do! OrderDetailRepository.insertAsync this.ConnectionString detail

            // Act
            do! OrderDetailRepository.updateShippedQuantityAsync this.ConnectionString "ORD001" 1 10

            // Assert
            let! details = OrderDetailRepository.findByOrderNoAsync this.ConnectionString "ORD001"
            let detailList = details |> Seq.toList
            detailList.[0].ShippedQuantity |> should equal 10
        }

    [<Fact>]
    member this.``完了フラグを更新できる``() =
        task {
            // Arrange
            do! setupCompany this.ConnectionString "COMP001"
            do! setupEmployee this.ConnectionString "EMP001"
            do! setupCustomer this.ConnectionString "COMP001" 1 "EMP001"
            do! setupOrder this.ConnectionString "ORD001" "COMP001" 1 "EMP001" "10000"
            do! setupProduct this.ConnectionString "PROD001"

            let detail = createTestDetail "ORD001" 1 "PROD001" "商品A" 10 1000
            do! OrderDetailRepository.insertAsync this.ConnectionString detail

            // Act
            do! OrderDetailRepository.updateCompletedAsync this.ConnectionString "ORD001" 1 1

            // Assert
            let! details = OrderDetailRepository.findByOrderNoAsync this.ConnectionString "ORD001"
            let detailList = details |> Seq.toList
            detailList.[0].IsCompleted |> should equal 1
        }

    [<Fact>]
    member this.``受注を削除すると明細もカスケード削除される``() =
        task {
            // Arrange
            do! setupCompany this.ConnectionString "COMP001"
            do! setupEmployee this.ConnectionString "EMP001"
            do! setupCustomer this.ConnectionString "COMP001" 1 "EMP001"
            do! setupOrder this.ConnectionString "ORD001" "COMP001" 1 "EMP001" "10000"
            do! setupProduct this.ConnectionString "PROD001"

            let detail = createTestDetail "ORD001" 1 "PROD001" "商品A" 10 1000
            do! OrderDetailRepository.insertAsync this.ConnectionString detail

            // Act
            do! OrderRepository.deleteAsync this.ConnectionString "ORD001"

            // Assert
            let! details = OrderDetailRepository.findByOrderNoAsync this.ConnectionString "ORD001"
            details |> Seq.length |> should equal 0
        }
