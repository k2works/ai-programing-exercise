module SalesManagement.Tests.IntegrationTests.ProductCategoryTests

open System
open Xunit
open FsUnit.Xunit
open SalesManagement.Domain.Models
open SalesManagement.Infrastructure.Repositories.ProductCategoryRepository
open SalesManagement.Tests.DatabaseTestBase

type ProductCategoryTests() =
    inherit DatabaseTestBase()

    let createTestCategory code name level path =
        {
            ProductCategoryCode = code
            ProductCategoryName = name
            ProductCategoryLevel = level
            ProductCategoryPath = path
            LowestLevelFlag = 1
            CreatedAt = DateTime.Now
            CreatedBy = "admin"
            UpdatedAt = DateTime.Now
            UpdatedBy = "admin"
        }

    [<Fact>]
    member this.``商品分類を登録できる``() =
        task {
            // Arrange
            let category = createTestCategory "CAT001" "電子機器" 1 "CAT001"

            // Act
            do! insertAsync this.ConnectionString category

            // Assert
            let! found = findByIdAsync this.ConnectionString "CAT001"
            found |> should not' (equal None)
            found.Value.ProductCategoryName |> should equal "電子機器"
        }

    [<Fact>]
    member this.``階層構造の商品分類を登録できる``() =
        task {
            // Arrange & Act
            // 第1階層
            let parent =
                { createTestCategory "CAT001" "電子機器" 1 "CAT001" with
                    LowestLevelFlag = 0 }
            do! insertAsync this.ConnectionString parent

            // 第2階層
            let child =
                { createTestCategory "CAT00101" "パソコン" 2 "CAT001/CAT00101" with
                    LowestLevelFlag = 0 }
            do! insertAsync this.ConnectionString child

            // 第3階層
            let grandChild =
                { createTestCategory "CAT0010101" "ノートPC" 3 "CAT001/CAT00101/CAT0010101" with
                    LowestLevelFlag = 1 }
            do! insertAsync this.ConnectionString grandChild

            // Assert
            let! categories = findAllAsync this.ConnectionString
            let catList = categories |> Seq.toList

            catList |> should haveLength 3
            catList.[0].ProductCategoryLevel |> should equal 1
            catList.[1].ProductCategoryLevel |> should equal 2
            catList.[2].ProductCategoryLevel |> should equal 3
        }

    [<Fact>]
    member this.``階層パスで配下の商品分類を検索できる``() =
        task {
            // Arrange
            do! insertAsync this.ConnectionString (createTestCategory "CAT001" "電子機器" 1 "CAT001")
            do! insertAsync this.ConnectionString (createTestCategory "CAT00101" "パソコン" 2 "CAT001/CAT00101")
            do! insertAsync this.ConnectionString (createTestCategory "CAT0010101" "ノートPC" 3 "CAT001/CAT00101/CAT0010101")
            do! insertAsync this.ConnectionString (createTestCategory "CAT002" "家具" 1 "CAT002")

            // Act
            let! underCat001 = findByPathPrefixAsync this.ConnectionString "CAT001"
            let resultList = underCat001 |> Seq.toList

            // Assert
            resultList |> should haveLength 3
            resultList |> List.forall (fun c -> c.ProductCategoryPath.StartsWith("CAT001")) |> should be True
        }

    [<Fact>]
    member this.``商品分類を更新できる``() =
        task {
            // Arrange
            let category = createTestCategory "CAT001" "電子機器" 1 "CAT001"
            do! insertAsync this.ConnectionString category

            // Act
            let updated =
                { category with
                    ProductCategoryName = "IT機器"
                    UpdatedAt = DateTime.Now
                    UpdatedBy = "updater" }
            do! updateAsync this.ConnectionString updated

            // Assert
            let! result = findByIdAsync this.ConnectionString "CAT001"
            result |> should not' (equal None)
            result.Value.ProductCategoryName |> should equal "IT機器"
            result.Value.UpdatedBy |> should equal "updater"
        }

    [<Fact>]
    member this.``商品分類を削除できる``() =
        task {
            // Arrange
            let category = createTestCategory "CAT001" "電子機器" 1 "CAT001"
            do! insertAsync this.ConnectionString category

            // Act
            do! deleteAsync this.ConnectionString "CAT001"

            // Assert
            let! deleted = findByIdAsync this.ConnectionString "CAT001"
            deleted |> should equal None
        }
