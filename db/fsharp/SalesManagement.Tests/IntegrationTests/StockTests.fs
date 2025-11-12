module SalesManagement.Tests.IntegrationTests.StockTests

open System
open Xunit
open FsUnit.Xunit
open SalesManagement.Domain.Models
open SalesManagement.Infrastructure.Repositories
open SalesManagement.Tests.DatabaseTestBase

type StockTests() =
    inherit DatabaseTestBase()

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

    let createTestStock warehouseCode productCode lotNo stockType qualityType actualQty validQty =
        {
            WarehouseCode = warehouseCode
            ProductCode = productCode
            LotNo = lotNo
            StockType = stockType
            QualityType = qualityType
            ActualQuantity = actualQty
            ValidQuantity = validQty
            LastDeliveryDate = None
            CreatedAt = DateTime.Now
            CreatedBy = "system"
            UpdatedAt = DateTime.Now
            UpdatedBy = "system"
        }

    [<Fact>]
    member this.``在庫を登録できる``() =
        task {
            // Arrange
            do! setupWarehouse this.ConnectionString "WH001"
            do! setupProductCategory this.ConnectionString "CAT001"
            do! setupProduct this.ConnectionString "PROD001"

            let stock = createTestStock "WH001" "PROD001" "LOT001" "1" "G" 100 100

            // Act
            do! StockRepository.insertAsync this.ConnectionString stock

            // Assert
            let! found = StockRepository.findByIdAsync this.ConnectionString "WH001" "PROD001" "LOT001" "1" "G"
            found |> should not' (equal None)
            found.Value.ActualQuantity |> should equal 100
            found.Value.ValidQuantity |> should equal 100
        }

    [<Fact>]
    member this.``倉庫コードで在庫を検索できる``() =
        task {
            // Arrange
            do! setupWarehouse this.ConnectionString "WH001"
            do! setupProductCategory this.ConnectionString "CAT001"
            do! setupProduct this.ConnectionString "PROD001"

            let stock1 = createTestStock "WH001" "PROD001" "LOT001" "1" "G" 100 100
            let stock2 = createTestStock "WH001" "PROD001" "LOT002" "1" "G" 50 50

            do! StockRepository.insertAsync this.ConnectionString stock1
            do! StockRepository.insertAsync this.ConnectionString stock2

            // Act
            let! stocks = StockRepository.findByWarehouseCodeAsync this.ConnectionString "WH001"

            // Assert
            let stockList = stocks |> Seq.toList
            stockList |> should haveLength 2
            stockList |> List.sumBy (fun s -> s.ActualQuantity) |> should equal 150
        }

    [<Fact>]
    member this.``商品コードで在庫を検索できる``() =
        task {
            // Arrange
            do! setupWarehouse this.ConnectionString "WH001"
            do! setupWarehouse this.ConnectionString "WH002"
            do! setupProductCategory this.ConnectionString "CAT001"
            do! setupProduct this.ConnectionString "PROD001"

            let stock1 = createTestStock "WH001" "PROD001" "LOT001" "1" "G" 100 100
            let stock2 = createTestStock "WH002" "PROD001" "LOT001" "1" "G" 50 50

            do! StockRepository.insertAsync this.ConnectionString stock1
            do! StockRepository.insertAsync this.ConnectionString stock2

            // Act
            let! stocks = StockRepository.findByProductCodeAsync this.ConnectionString "PROD001"

            // Assert
            let stockList = stocks |> Seq.toList
            stockList |> should haveLength 2
            stockList |> List.sumBy (fun s -> s.ActualQuantity) |> should equal 150
        }

    [<Fact>]
    member this.``有効在庫数を更新できる``() =
        task {
            // Arrange
            do! setupWarehouse this.ConnectionString "WH001"
            do! setupProductCategory this.ConnectionString "CAT001"
            do! setupProduct this.ConnectionString "PROD001"

            let stock = createTestStock "WH001" "PROD001" "LOT001" "1" "G" 100 100
            do! StockRepository.insertAsync this.ConnectionString stock

            // Act
            do! StockRepository.updateValidQuantityAsync this.ConnectionString "WH001" "PROD001" "LOT001" "1" "G" 80

            // Assert
            let! found = StockRepository.findByIdAsync this.ConnectionString "WH001" "PROD001" "LOT001" "1" "G"
            found.Value.ActualQuantity |> should equal 100
            found.Value.ValidQuantity |> should equal 80
        }

    [<Fact>]
    member this.``在庫を削除できる``() =
        task {
            // Arrange
            do! setupWarehouse this.ConnectionString "WH001"
            do! setupProductCategory this.ConnectionString "CAT001"
            do! setupProduct this.ConnectionString "PROD001"

            let stock = createTestStock "WH001" "PROD001" "LOT001" "1" "G" 100 100
            do! StockRepository.insertAsync this.ConnectionString stock

            // Act
            do! StockRepository.deleteAsync this.ConnectionString "WH001" "PROD001" "LOT001" "1" "G"

            // Assert
            let! deleted = StockRepository.findByIdAsync this.ConnectionString "WH001" "PROD001" "LOT001" "1" "G"
            deleted |> should equal None
        }

    [<Fact>]
    member this.``良品区分ごとに在庫を管理できる``() =
        task {
            // Arrange
            do! setupWarehouse this.ConnectionString "WH001"
            do! setupProductCategory this.ConnectionString "CAT001"
            do! setupProduct this.ConnectionString "PROD001"

            let goodStock = createTestStock "WH001" "PROD001" "LOT001" "1" "G" 100 100
            let badStock = createTestStock "WH001" "PROD001" "LOT001" "1" "B" 10 0
            let holdStock = createTestStock "WH001" "PROD001" "LOT001" "1" "H" 5 0

            do! StockRepository.insertAsync this.ConnectionString goodStock
            do! StockRepository.insertAsync this.ConnectionString badStock
            do! StockRepository.insertAsync this.ConnectionString holdStock

            // Act
            let! foundGood = StockRepository.findByIdAsync this.ConnectionString "WH001" "PROD001" "LOT001" "1" "G"
            let! foundBad = StockRepository.findByIdAsync this.ConnectionString "WH001" "PROD001" "LOT001" "1" "B"
            let! foundHold = StockRepository.findByIdAsync this.ConnectionString "WH001" "PROD001" "LOT001" "1" "H"

            // Assert
            foundGood.Value.QualityType |> should equal "G"
            foundGood.Value.ValidQuantity |> should equal 100
            foundBad.Value.QualityType |> should equal "B"
            foundBad.Value.ValidQuantity |> should equal 0
            foundHold.Value.QualityType |> should equal "H"
            foundHold.Value.ValidQuantity |> should equal 0
        }
