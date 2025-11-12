module SalesManagement.Tests.IntegrationTests.PurchaseOrderTests

open System
open Xunit
open FsUnit.Xunit
open SalesManagement.Domain.Models
open SalesManagement.Infrastructure.Repositories
open SalesManagement.Tests.DatabaseTestBase

type PurchaseOrderTests() =
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

    let createTestPurchaseOrder poNo orderNo supplierCode supplierBranch employeeCode warehouseCode amount =
        {
            PoNo = poNo
            PoDate = DateTime.Now
            OrderNo = orderNo
            SupplierCode = supplierCode
            SupplierBranch = supplierBranch
            EmployeeCode = employeeCode
            DueDate = Some(DateTime.Now.AddDays(14.0))
            WarehouseCode = warehouseCode
            PoAmount = amount
            ConsumptionTax = amount / 10
            SlipComment = None
            CreatedAt = DateTime.Now
            CreatedBy = "system"
            UpdatedAt = DateTime.Now
            UpdatedBy = "system"
        }

    [<Fact>]
    member this.``発注を登録できる``() =
        task {
            // Arrange
            do! setupCompany this.ConnectionString "COMP001"
            do! setupEmployee this.ConnectionString "EMP001"
            do! setupCustomer this.ConnectionString "COMP001" 1 "EMP001"
            do! setupSupplier this.ConnectionString "SUPP001" 1 "EMP001"
            do! setupOrder this.ConnectionString "ORD001" "COMP001" 1 "EMP001"
            do! setupWarehouse this.ConnectionString "WH001"

            let po = createTestPurchaseOrder "PO001" "ORD001" "SUPP001" 1 "EMP001" "WH001" 50000

            // Act
            do! PurchaseOrderRepository.insertAsync this.ConnectionString po

            // Assert
            let! found = PurchaseOrderRepository.findByIdAsync this.ConnectionString "PO001"
            found |> should not' (equal None)
            found.Value.PoNo |> should equal "PO001"
            found.Value.OrderNo |> should equal "ORD001"
            found.Value.PoAmount |> should equal 50000
        }

    [<Fact>]
    member this.``受注番号から発注を検索できる``() =
        task {
            // Arrange
            do! setupCompany this.ConnectionString "COMP001"
            do! setupEmployee this.ConnectionString "EMP001"
            do! setupCustomer this.ConnectionString "COMP001" 1 "EMP001"
            do! setupSupplier this.ConnectionString "SUPP001" 1 "EMP001"
            do! setupOrder this.ConnectionString "ORD001" "COMP001" 1 "EMP001"
            do! setupWarehouse this.ConnectionString "WH001"

            let po1 = createTestPurchaseOrder "PO001" "ORD001" "SUPP001" 1 "EMP001" "WH001" 50000
            let po2 = createTestPurchaseOrder "PO002" "ORD001" "SUPP001" 1 "EMP001" "WH001" 30000

            do! PurchaseOrderRepository.insertAsync this.ConnectionString po1
            do! PurchaseOrderRepository.insertAsync this.ConnectionString po2

            // Act
            let! orders = PurchaseOrderRepository.findByOrderNoAsync this.ConnectionString "ORD001"

            // Assert
            let orderList = orders |> Seq.toList
            orderList |> should haveLength 2
            orderList |> List.sumBy (fun o -> o.PoAmount) |> should equal 80000
        }

    [<Fact>]
    member this.``発注を削除できる``() =
        task {
            // Arrange
            do! setupCompany this.ConnectionString "COMP001"
            do! setupEmployee this.ConnectionString "EMP001"
            do! setupCustomer this.ConnectionString "COMP001" 1 "EMP001"
            do! setupSupplier this.ConnectionString "SUPP001" 1 "EMP001"
            do! setupOrder this.ConnectionString "ORD001" "COMP001" 1 "EMP001"
            do! setupWarehouse this.ConnectionString "WH001"

            let po = createTestPurchaseOrder "PO001" "ORD001" "SUPP001" 1 "EMP001" "WH001" 50000
            do! PurchaseOrderRepository.insertAsync this.ConnectionString po

            // Act
            do! PurchaseOrderRepository.deleteAsync this.ConnectionString "PO001"

            // Assert
            let! deleted = PurchaseOrderRepository.findByIdAsync this.ConnectionString "PO001"
            deleted |> should equal None
        }
