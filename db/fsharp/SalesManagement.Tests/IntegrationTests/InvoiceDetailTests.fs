module SalesManagement.Tests.IntegrationTests.InvoiceDetailTests

open System
open Xunit
open FsUnit.Xunit
open SalesManagement.Domain.Models
open SalesManagement.Infrastructure.Repositories
open SalesManagement.Tests.DatabaseTestBase

type InvoiceDetailTests() =
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

    let setupSales (connectionString: string) salesSlipNo orderNo customerCode customerBranch employeeCode =
        task {
            let sales = {
                SalesSlipNo = salesSlipNo
                SalesDate = DateTime.Now
                OrderNo = Some orderNo
                CustomerCode = customerCode
                CustomerBranch = customerBranch
                EmployeeCode = employeeCode
                SalesAmount = 100000
                ConsumptionTax = 10000
                SlipComment = None
                DepartmentCode = "10000"
                CreatedAt = DateTime.Now
                CreatedBy = "system"
                UpdatedAt = DateTime.Now
                UpdatedBy = "system"
            }
            do! SalesRepository.insertAsync connectionString sales
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

    let setupSalesDetail (connectionString: string) salesSlipNo lineNo productCode =
        task {
            let detail = {
                SalesSlipNo = salesSlipNo
                SalesLineNo = lineNo
                ProductCode = productCode
                ProductName = "テスト商品"
                Quantity = 100
                UnitPrice = 500
                Amount = 50000
                ConsumptionTax = 5000
                CreatedAt = DateTime.Now
                CreatedBy = "system"
                UpdatedAt = DateTime.Now
                UpdatedBy = "system"
            }
            do! SalesDetailRepository.insertAsync connectionString detail
        }

    let setupInvoice (connectionString: string) invoiceNo customerCode customerBranch =
        task {
            let invoice = {
                InvoiceNo = invoiceNo
                InvoiceDate = DateTime.Now
                CustomerCode = customerCode
                CustomerBranch = customerBranch
                SalesSlipNo = None
                InvoiceAmount = 100000m
                ClearedAmount = 0m
                Remarks = None
                DepartmentCode = "10000"
                CreatedAt = DateTime.Now
                CreatedBy = "system"
                UpdatedAt = DateTime.Now
                UpdatedBy = "system"
            }
            do! InvoiceRepository.insertAsync connectionString invoice
        }

    let createTestInvoiceDetail invoiceNo detailNo salesSlipNo salesDetailNo amount =
        {
            InvoiceNo = invoiceNo
            InvoiceDetailNo = detailNo
            SalesSlipNo = salesSlipNo
            SalesDetailNo = salesDetailNo
            InvoiceAmount = amount
            CreatedAt = DateTime.Now
            CreatedBy = "system"
            UpdatedAt = DateTime.Now
            UpdatedBy = "system"
        }

    [<Fact>]
    member this.``請求明細を登録できる``() =
        task {
            // Arrange
            do! setupCompany this.ConnectionString "COMP001"
            do! setupEmployee this.ConnectionString "EMP001"
            do! setupCustomer this.ConnectionString "COMP001" 1 "EMP001"
            do! setupOrder this.ConnectionString "ORD001" "COMP001" 1 "EMP001"
            do! setupSales this.ConnectionString "SAL001" "ORD001" "COMP001" 1 "EMP001"
            do! setupProductCategory this.ConnectionString "CAT001"
            do! setupProduct this.ConnectionString "PROD001"
            do! setupSalesDetail this.ConnectionString "SAL001" 1 "PROD001"
            do! setupInvoice this.ConnectionString "INV001" "COMP001" 1

            let detail1 = createTestInvoiceDetail "INV001" 1 "SAL001" 1 60000m
            let detail2 = createTestInvoiceDetail "INV001" 2 "SAL001" 1 40000m

            // Act
            do! InvoiceDetailRepository.insertAsync this.ConnectionString detail1
            do! InvoiceDetailRepository.insertAsync this.ConnectionString detail2

            // Assert
            let! details = InvoiceDetailRepository.findByInvoiceNoAsync this.ConnectionString "INV001"
            let detailList = details |> Seq.toList
            detailList |> should haveLength 2
            detailList |> List.sumBy (fun d -> d.InvoiceAmount) |> should equal 100000m
        }

    [<Fact>]
    member this.``請求明細を削除できる``() =
        task {
            // Arrange
            do! setupCompany this.ConnectionString "COMP001"
            do! setupEmployee this.ConnectionString "EMP001"
            do! setupCustomer this.ConnectionString "COMP001" 1 "EMP001"
            do! setupOrder this.ConnectionString "ORD001" "COMP001" 1 "EMP001"
            do! setupSales this.ConnectionString "SAL001" "ORD001" "COMP001" 1 "EMP001"
            do! setupProductCategory this.ConnectionString "CAT001"
            do! setupProduct this.ConnectionString "PROD001"
            do! setupSalesDetail this.ConnectionString "SAL001" 1 "PROD001"
            do! setupInvoice this.ConnectionString "INV001" "COMP001" 1

            let detail = createTestInvoiceDetail "INV001" 1 "SAL001" 1 100000m
            do! InvoiceDetailRepository.insertAsync this.ConnectionString detail

            // Act
            do! InvoiceDetailRepository.deleteByInvoiceNoAsync this.ConnectionString "INV001"

            // Assert
            let! details = InvoiceDetailRepository.findByInvoiceNoAsync this.ConnectionString "INV001"
            let detailList = details |> Seq.toList
            detailList |> should haveLength 0
        }
