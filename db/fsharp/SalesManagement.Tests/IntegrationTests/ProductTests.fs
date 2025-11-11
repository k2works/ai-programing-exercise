module SalesManagement.Tests.IntegrationTests.ProductTests

open System
open Xunit
open FsUnit.Xunit
open SalesManagement.Domain.Models
open SalesManagement.Infrastructure.Repositories
open SalesManagement.Infrastructure.Repositories.ProductRepository
open SalesManagement.Tests.DatabaseTestBase

type ProductTests() =
    inherit DatabaseTestBase()

    let createTestCategory code name =
        {
            ProductCategoryCode = code
            ProductCategoryName = name
            ProductCategoryLevel = 1
            ProductCategoryPath = code
            LowestLevelFlag = 1
            CreatedAt = DateTime.Now
            CreatedBy = "admin"
            UpdatedAt = DateTime.Now
            UpdatedBy = "admin"
        }

    let createTestProduct code name categoryCode =
        {
            ProductCode = code
            ProductFormalName = name
            ProductAbbreviation = name
            ProductNameKana = "テストショウヒン"
            ProductType = "製品"
            ModelNumber = Some "MODEL-001"
            SellingPrice = 150000
            PurchasePrice = 100000
            CostOfSales = 100000
            TaxType = 1
            ProductCategoryCode = categoryCode
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

    [<Fact>]
    member this.``商品を登録できる``() =
        task {
            // Arrange
            let category = createTestCategory "CAT001" "電子機器"
            do! ProductCategoryRepository.insertAsync this.ConnectionString category

            let product = createTestProduct "PROD001" "ノートパソコン A型" "CAT001"

            // Act
            do! insertAsync this.ConnectionString product

            // Assert
            let! found = findByIdAsync this.ConnectionString "PROD001"
            found |> should not' (equal None)
            found.Value.ProductFormalName |> should equal "ノートパソコン A型"
            found.Value.SellingPrice |> should equal 150000
        }

    [<Fact>]
    member this.``商品分類コードで商品を検索できる``() =
        task {
            // Arrange
            let category = createTestCategory "CAT001" "電子機器"
            do! ProductCategoryRepository.insertAsync this.ConnectionString category

            do! insertAsync this.ConnectionString (createTestProduct "PROD001" "ノートPC A型" "CAT001")
            do! insertAsync this.ConnectionString (createTestProduct "PROD002" "ノートPC B型" "CAT001")

            // Act
            let! products = findByCategoryAsync this.ConnectionString "CAT001"
            let productList = products |> Seq.toList

            // Assert
            productList |> should haveLength 2
            productList |> List.forall (fun p -> p.ProductCategoryCode = "CAT001") |> should be True
        }

    [<Fact>]
    member this.``商品を更新できる``() =
        task {
            // Arrange
            let category = createTestCategory "CAT001" "電子機器"
            do! ProductCategoryRepository.insertAsync this.ConnectionString category

            let product = createTestProduct "PROD001" "ノートPC A型" "CAT001"
            do! insertAsync this.ConnectionString product

            // Act
            let updated =
                { product with
                    ProductFormalName = "ノートPC A型 改良版"
                    SellingPrice = 180000
                    UpdatedAt = DateTime.Now
                    UpdatedBy = "updater" }
            do! updateAsync this.ConnectionString updated

            // Assert
            let! result = findByIdAsync this.ConnectionString "PROD001"
            result |> should not' (equal None)
            result.Value.ProductFormalName |> should equal "ノートPC A型 改良版"
            result.Value.SellingPrice |> should equal 180000
        }
