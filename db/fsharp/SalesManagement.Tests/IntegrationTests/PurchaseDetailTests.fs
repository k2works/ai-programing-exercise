module SalesManagement.Tests.IntegrationTests.PurchaseDetailTests

open System
open Xunit
open FsUnit.Xunit
open SalesManagement.Domain.Models
open SalesManagement.Infrastructure.Repositories
open SalesManagement.Tests.DatabaseTestBase

type PurchaseDetailTests() =
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

    let setupPurchase (connectionString: string) purchaseNo poNo supplierCode supplierBranch employeeCode =
        task {
            let purchase = {
                PurchaseNo = purchaseNo
                PurchaseDate = DateTime.Now
                PoNo = poNo
                SupplierCode = supplierCode
                SupplierBranch = supplierBranch
                EmployeeCode = employeeCode
                PurchaseAmount = 50000
                ConsumptionTax = 5000
                SlipComment = None
                DepartmentCode = "10000"
                CreatedAt = DateTime.Now
                CreatedBy = "system"
                UpdatedAt = DateTime.Now
                UpdatedBy = "system"
            }
            do! PurchaseRepository.insertAsync connectionString purchase
        }

    let setupProductCategory (connectionString: string) categoryCode =
        task {
            let category = {
                ProductCategoryCode = categoryCode
                ProductCategoryName = "テストカテゴリ"
                ProductCategoryLevel = 1
                ProductCategoryPath = categoryCode
                LowestLevelFlag = 1
                CreatedAt = DateTime.Now
                CreatedBy = "admin"
                UpdatedAt = DateTime.Now
                UpdatedBy = "admin"
            }
            do! ProductCategoryRepository.insertAsync connectionString category
        }

    let setupProduct (connectionString: string) productCode =
        task {
            let product = {
                ProductCode = productCode
                ProductFormalName = "テスト商品"
                ProductAbbreviation = "テスト商品"
                ProductNameKana = "テストショウヒン"
                ProductType = "製品"
                ModelNumber = None
                SellingPrice = 1000
                PurchasePrice = 800
                CostOfSales = 800
                TaxType = 1
                ProductCategoryCode = "CAT001"
                MiscellaneousType = 0
                InventoryManagementFlag = 1
                InventoryAllocationFlag = 0
                SupplierCode = None
                SupplierBranch = None
                CreatedAt = DateTime.Now
                CreatedBy = "admin"
                UpdatedAt = DateTime.Now
                UpdatedBy = "admin"
            }
            do! ProductRepository.insertAsync connectionString product
        }

    let createTestPurchaseDetail purchaseNo lineNo productCode productName lotNo warehouseCode quantity unitPrice =
        {
            PurchaseNo = purchaseNo
            PurchaseLineNo = lineNo
            ProductCode = productCode
            ProductName = productName
            LotNo = lotNo
            WarehouseCode = warehouseCode
            PurchaseQuantity = quantity
            UnitPrice = unitPrice
            Amount = quantity * unitPrice
            ConsumptionTax = (quantity * unitPrice) / 10
            CreatedAt = DateTime.Now
            CreatedBy = "system"
            UpdatedAt = DateTime.Now
            UpdatedBy = "system"
        }

    [<Fact>]
    member this.``仕入明細を登録できる``() =
        task {
            // Arrange
            do! setupCompany this.ConnectionString "COMP001"
            do! setupEmployee this.ConnectionString "EMP001"
            do! setupCustomer this.ConnectionString "COMP001" 1 "EMP001"
            do! setupSupplier this.ConnectionString "SUPP001" 1 "EMP001"
            do! setupOrder this.ConnectionString "ORD001" "COMP001" 1 "EMP001"
            do! setupWarehouse this.ConnectionString "WH001"
            do! setupPurchaseOrder this.ConnectionString "PO001" "ORD001" "SUPP001" 1 "EMP001" "WH001"
            do! setupPurchase this.ConnectionString "PUR001" "PO001" "SUPP001" 1 "EMP001"
            do! setupProductCategory this.ConnectionString "CAT001"
            do! setupProduct this.ConnectionString "PROD001"

            let detail = createTestPurchaseDetail "PUR001" 1 "PROD001" "テスト商品" "LOT001" "WH001" 100 500

            // Act
            do! PurchaseDetailRepository.insertAsync this.ConnectionString detail

            // Assert
            let! details = PurchaseDetailRepository.findByPurchaseNoAsync this.ConnectionString "PUR001"
            let detailList = details |> Seq.toList
            detailList |> should haveLength 1
            detailList.[0].ProductCode |> should equal "PROD001"
            detailList.[0].LotNo |> should equal "LOT001"
            detailList.[0].PurchaseQuantity |> should equal 100
        }

    [<Fact>]
    member this.``仕入明細を削除できる``() =
        task {
            // Arrange
            do! setupCompany this.ConnectionString "COMP001"
            do! setupEmployee this.ConnectionString "EMP001"
            do! setupCustomer this.ConnectionString "COMP001" 1 "EMP001"
            do! setupSupplier this.ConnectionString "SUPP001" 1 "EMP001"
            do! setupOrder this.ConnectionString "ORD001" "COMP001" 1 "EMP001"
            do! setupWarehouse this.ConnectionString "WH001"
            do! setupPurchaseOrder this.ConnectionString "PO001" "ORD001" "SUPP001" 1 "EMP001" "WH001"
            do! setupPurchase this.ConnectionString "PUR001" "PO001" "SUPP001" 1 "EMP001"
            do! setupProductCategory this.ConnectionString "CAT001"
            do! setupProduct this.ConnectionString "PROD001"

            let detail = createTestPurchaseDetail "PUR001" 1 "PROD001" "テスト商品" "LOT001" "WH001" 100 500
            do! PurchaseDetailRepository.insertAsync this.ConnectionString detail

            // Act
            do! PurchaseDetailRepository.deleteByPurchaseNoAsync this.ConnectionString "PUR001"

            // Assert
            let! details = PurchaseDetailRepository.findByPurchaseNoAsync this.ConnectionString "PUR001"
            let detailList = details |> Seq.toList
            detailList |> should haveLength 0
        }

    [<Fact>]
    member this.``複数の仕入明細を登録できる``() =
        task {
            // Arrange
            do! setupCompany this.ConnectionString "COMP001"
            do! setupEmployee this.ConnectionString "EMP001"
            do! setupCustomer this.ConnectionString "COMP001" 1 "EMP001"
            do! setupSupplier this.ConnectionString "SUPP001" 1 "EMP001"
            do! setupOrder this.ConnectionString "ORD001" "COMP001" 1 "EMP001"
            do! setupWarehouse this.ConnectionString "WH001"
            do! setupPurchaseOrder this.ConnectionString "PO001" "ORD001" "SUPP001" 1 "EMP001" "WH001"
            do! setupPurchase this.ConnectionString "PUR001" "PO001" "SUPP001" 1 "EMP001"
            do! setupProductCategory this.ConnectionString "CAT001"
            do! setupProduct this.ConnectionString "PROD001"

            let detail1 = createTestPurchaseDetail "PUR001" 1 "PROD001" "テスト商品" "LOT001" "WH001" 100 500
            let detail2 = createTestPurchaseDetail "PUR001" 2 "PROD001" "テスト商品" "LOT002" "WH001" 50 500

            do! PurchaseDetailRepository.insertAsync this.ConnectionString detail1
            do! PurchaseDetailRepository.insertAsync this.ConnectionString detail2

            // Act
            let! details = PurchaseDetailRepository.findByPurchaseNoAsync this.ConnectionString "PUR001"

            // Assert
            let detailList = details |> Seq.toList
            detailList |> should haveLength 2
            detailList |> List.sumBy (fun d -> d.PurchaseQuantity) |> should equal 150
        }

    [<Fact>]
    member this.``仕入明細のロット番号と倉庫コードが記録される``() =
        task {
            // Arrange
            do! setupCompany this.ConnectionString "COMP001"
            do! setupEmployee this.ConnectionString "EMP001"
            do! setupCustomer this.ConnectionString "COMP001" 1 "EMP001"
            do! setupSupplier this.ConnectionString "SUPP001" 1 "EMP001"
            do! setupOrder this.ConnectionString "ORD001" "COMP001" 1 "EMP001"
            do! setupWarehouse this.ConnectionString "WH001"
            do! setupPurchaseOrder this.ConnectionString "PO001" "ORD001" "SUPP001" 1 "EMP001" "WH001"
            do! setupPurchase this.ConnectionString "PUR001" "PO001" "SUPP001" 1 "EMP001"
            do! setupProductCategory this.ConnectionString "CAT001"
            do! setupProduct this.ConnectionString "PROD001"

            let detail = createTestPurchaseDetail "PUR001" 1 "PROD001" "テスト商品" "LOT20250106-001" "WH001" 100 500

            // Act
            do! PurchaseDetailRepository.insertAsync this.ConnectionString detail

            // Assert
            let! details = PurchaseDetailRepository.findByPurchaseNoAsync this.ConnectionString "PUR001"
            let detailList = details |> Seq.toList
            detailList.[0].LotNo |> should equal "LOT20250106-001"
            detailList.[0].WarehouseCode |> should equal "WH001"
        }
