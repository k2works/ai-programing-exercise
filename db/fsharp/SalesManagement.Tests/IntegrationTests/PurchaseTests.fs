module SalesManagement.Tests.IntegrationTests.PurchaseTests

open System
open Xunit
open FsUnit.Xunit
open SalesManagement.Domain.Models
open SalesManagement.Infrastructure.Repositories
open SalesManagement.Tests.DatabaseTestBase

type PurchaseTests() =
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

    let setupSupplier (connectionString: string) supplierCode supplierBranch employeeCode =
        task {
            do! setupCompany connectionString supplierCode

            let supplier = {
                SupplierCode = supplierCode
                SupplierBranch = supplierBranch
                SupplierType = 0
                SupplierName = "テスト仕入先"
                SupplierNameKana = None
                EmployeeCode = employeeCode
                SupplierCloseDate = 31
                SupplierPayMonths = 1
                SupplierPayDates = Some 31
                SupplierPayMethod = 1
                CreatedAt = DateTime.Now
                CreatedBy = "admin"
                UpdatedAt = DateTime.Now
                UpdatedBy = "admin"
            }
            do! SupplierRepository.insertAsync connectionString supplier
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

    let setupOrder (connectionString: string) orderNo customerCode customerBranch employeeCode =
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
                DepartmentCode = "10000"
                CreatedAt = DateTime.Now
                CreatedBy = "system"
                UpdatedAt = DateTime.Now
                UpdatedBy = "system"
            }
            do! OrderRepository.insertAsync connectionString order
        }

    let setupWarehouse (connectionString: string) warehouseCode =
        task {
            let warehouse = {
                WarehouseCode = warehouseCode
                WarehouseName = "第一倉庫"
                WarehouseType = 1
                Address = None
                PhoneNumber = None
                ManagerCode = None
                CreatedAt = DateTime.Now
                CreatedBy = "system"
                UpdatedAt = DateTime.Now
                UpdatedBy = "system"
            }
            do! WarehouseRepository.insertAsync connectionString warehouse
        }

    let setupPurchaseOrder (connectionString: string) poNo orderNo supplierCode supplierBranch employeeCode warehouseCode =
        task {
            let po = {
                PoNo = poNo
                PoDate = DateTime.Now
                OrderNo = orderNo
                SupplierCode = supplierCode
                SupplierBranch = supplierBranch
                EmployeeCode = employeeCode
                DueDate = Some(DateTime.Now.AddDays(14.0))
                WarehouseCode = warehouseCode
                PoAmount = 50000
                ConsumptionTax = 5000
                SlipComment = None
                CreatedAt = DateTime.Now
                CreatedBy = "system"
                UpdatedAt = DateTime.Now
                UpdatedBy = "system"
            }
            do! PurchaseOrderRepository.insertAsync connectionString po
        }

    let createTestPurchase purchaseNo poNo supplierCode supplierBranch employeeCode amount =
        {
            PurchaseNo = purchaseNo
            PurchaseDate = DateTime.Now
            PoNo = poNo
            SupplierCode = supplierCode
            SupplierBranch = supplierBranch
            EmployeeCode = employeeCode
            PurchaseAmount = amount
            ConsumptionTax = amount / 10
            SlipComment = None
            DepartmentCode = "10000"
            CreatedAt = DateTime.Now
            CreatedBy = "system"
            UpdatedAt = DateTime.Now
            UpdatedBy = "system"
        }

    [<Fact>]
    member this.``仕入を登録できる``() =
        task {
            // Arrange
            do! setupCompany this.ConnectionString "COMP001"
            do! setupEmployee this.ConnectionString "EMP001"
            do! setupCustomer this.ConnectionString "COMP001" 1 "EMP001"
            do! setupSupplier this.ConnectionString "SUPP001" 1 "EMP001"
            do! setupOrder this.ConnectionString "ORD001" "COMP001" 1 "EMP001"
            do! setupWarehouse this.ConnectionString "WH001"
            do! setupPurchaseOrder this.ConnectionString "PO001" "ORD001" "SUPP001" 1 "EMP001" "WH001"

            let purchase = createTestPurchase "PUR001" "PO001" "SUPP001" 1 "EMP001" 50000

            // Act
            do! PurchaseRepository.insertAsync this.ConnectionString purchase

            // Assert
            let! found = PurchaseRepository.findByIdAsync this.ConnectionString "PUR001"
            found |> should not' (equal None)
            found.Value.PurchaseNo |> should equal "PUR001"
            found.Value.PoNo |> should equal "PO001"
            found.Value.PurchaseAmount |> should equal 50000
        }

    [<Fact>]
    member this.``発注番号から仕入を検索できる``() =
        task {
            // Arrange
            do! setupCompany this.ConnectionString "COMP001"
            do! setupEmployee this.ConnectionString "EMP001"
            do! setupCustomer this.ConnectionString "COMP001" 1 "EMP001"
            do! setupSupplier this.ConnectionString "SUPP001" 1 "EMP001"
            do! setupOrder this.ConnectionString "ORD001" "COMP001" 1 "EMP001"
            do! setupWarehouse this.ConnectionString "WH001"
            do! setupPurchaseOrder this.ConnectionString "PO001" "ORD001" "SUPP001" 1 "EMP001" "WH001"

            let purchase1 = createTestPurchase "PUR001" "PO001" "SUPP001" 1 "EMP001" 30000
            let purchase2 = createTestPurchase "PUR002" "PO001" "SUPP001" 1 "EMP001" 20000

            do! PurchaseRepository.insertAsync this.ConnectionString purchase1
            do! PurchaseRepository.insertAsync this.ConnectionString purchase2

            // Act
            let! purchases = PurchaseRepository.findByPoNoAsync this.ConnectionString "PO001"

            // Assert
            let purchaseList = purchases |> Seq.toList
            purchaseList |> should haveLength 2
            purchaseList |> List.sumBy (fun p -> p.PurchaseAmount) |> should equal 50000
        }

    [<Fact>]
    member this.``仕入を削除できる``() =
        task {
            // Arrange
            do! setupCompany this.ConnectionString "COMP001"
            do! setupEmployee this.ConnectionString "EMP001"
            do! setupCustomer this.ConnectionString "COMP001" 1 "EMP001"
            do! setupSupplier this.ConnectionString "SUPP001" 1 "EMP001"
            do! setupOrder this.ConnectionString "ORD001" "COMP001" 1 "EMP001"
            do! setupWarehouse this.ConnectionString "WH001"
            do! setupPurchaseOrder this.ConnectionString "PO001" "ORD001" "SUPP001" 1 "EMP001" "WH001"

            let purchase = createTestPurchase "PUR001" "PO001" "SUPP001" 1 "EMP001" 50000
            do! PurchaseRepository.insertAsync this.ConnectionString purchase

            // Act
            do! PurchaseRepository.deleteAsync this.ConnectionString "PUR001"

            // Assert
            let! deleted = PurchaseRepository.findByIdAsync this.ConnectionString "PUR001"
            deleted |> should equal None
        }
