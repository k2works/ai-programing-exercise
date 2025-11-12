module ProductControllerTests

open System
open System.Net
open System.Net.Http
open System.Text
open System.Text.Json
open Xunit
open FsUnit.Xunit
open Microsoft.AspNetCore.Mvc.Testing
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Configuration
open Npgsql
open Dapper
open SalesManagement.Api.Dtos

/// テスト用の WebApplicationFactory
type ApiTestFactory() =
    inherit WebApplicationFactory<Program>()

/// テスト用のヘルパー関数
module TestHelpers =
    let getConnectionString () =
        "Host=localhost;Port=5432;Database=sales_management_fsharp;Username=postgres;Password=postgres"

    /// データベースをクリーンアップ
    let cleanupDatabase () = task {
        use connection = new NpgsqlConnection(getConnectionString())
        do! connection.ExecuteAsync("DELETE FROM 商品マスタ") |> Async.AwaitTask |> Async.Ignore
    }

    /// テスト用商品データを作成
    let createTestProduct code formalName =
        {| ProductCode = code
           ProductFormalName = formalName
           ProductAbbreviation = "略称"
           ProductNameKana = "カナ"
           ProductType = "TYPE001"
           ModelNumber = Some "MODEL001"
           SellingPrice = 1000
           PurchasePrice = 800
           CostOfSales = 850
           TaxType = 0
           ProductCategoryCode = "CAT001"
           MiscellaneousType = 0
           InventoryManagementFlag = 1
           InventoryAllocationFlag = 1
           SupplierCode = Some "SUP001"
           SupplierBranch = Some 1 |}

    /// JSON にシリアライズ
    let toJson obj =
        JsonSerializer.Serialize(obj)

    /// JSON から デシリアライズ
    let fromJson<'T> (json: string) =
        JsonSerializer.Deserialize<'T>(json)

    /// HttpContent を作成
    let createJsonContent obj =
        new StringContent(toJson obj, Encoding.UTF8, "application/json")

/// 統合テスト
type ProductControllerIntegrationTests(factory: ApiTestFactory) =
    // テスト前にデータベースをクリーンアップ
    do TestHelpers.cleanupDatabase() |> Async.AwaitTask |> Async.RunSynchronously

    interface IClassFixture<ApiTestFactory>

    [<Fact>]
    member _.``POST api/products - 商品を作成できる`` () = task {
        // Arrange
        use client = factory.CreateClient()
        let product = TestHelpers.createTestProduct "PROD001" "テスト商品1"
        let content = TestHelpers.createJsonContent product

        // Act
        let! response = client.PostAsync("/api/products", content)

        // Assert
        response.StatusCode |> should equal HttpStatusCode.Created
        let! body = response.Content.ReadAsStringAsync()
        let result = TestHelpers.fromJson<ProductResponse>(body)
        result.ProductCode |> should equal "PROD001"
        result.ProductFormalName |> should equal "テスト商品1"
    }

    [<Fact>]
    member _.``POST api/products - 重複する商品コードでエラー`` () = task {
        // Arrange
        use client = factory.CreateClient()
        let product = TestHelpers.createTestProduct "PROD002" "テスト商品2"
        let content1 = TestHelpers.createJsonContent product
        let content2 = TestHelpers.createJsonContent product

        // 最初の商品を作成
        let! _ = client.PostAsync("/api/products", content1)

        // Act - 同じ商品コードで再度作成を試みる
        let! response = client.PostAsync("/api/products", content2)

        // Assert
        response.StatusCode |> should equal HttpStatusCode.BadRequest
    }

    [<Fact>]
    member _.``GET api/products - すべての商品を取得できる`` () = task {
        // Arrange
        use client = factory.CreateClient()
        let product1 = TestHelpers.createTestProduct "PROD003" "テスト商品3"
        let product2 = TestHelpers.createTestProduct "PROD004" "テスト商品4"
        let! _ = client.PostAsync("/api/products", TestHelpers.createJsonContent product1)
        let! _ = client.PostAsync("/api/products", TestHelpers.createJsonContent product2)

        // Act
        let! response = client.GetAsync("/api/products")

        // Assert
        response.StatusCode |> should equal HttpStatusCode.OK
        let! body = response.Content.ReadAsStringAsync()
        let results = TestHelpers.fromJson<ProductResponse list>(body)
        results.Length |> should be (greaterThan 0)
    }

    [<Fact>]
    member _.``GET api/products/{productCode} - 商品を取得できる`` () = task {
        // Arrange
        use client = factory.CreateClient()
        let product = TestHelpers.createTestProduct "PROD005" "テスト商品5"
        let! _ = client.PostAsync("/api/products", TestHelpers.createJsonContent product)

        // Act
        let! response = client.GetAsync("/api/products/PROD005")

        // Assert
        response.StatusCode |> should equal HttpStatusCode.OK
        let! body = response.Content.ReadAsStringAsync()
        let result = TestHelpers.fromJson<ProductResponse>(body)
        result.ProductCode |> should equal "PROD005"
        result.ProductFormalName |> should equal "テスト商品5"
    }

    [<Fact>]
    member _.``GET api/products/{productCode} - 存在しない商品で404`` () = task {
        // Arrange
        use client = factory.CreateClient()

        // Act
        let! response = client.GetAsync("/api/products/NOTFOUND")

        // Assert
        response.StatusCode |> should equal HttpStatusCode.NotFound
    }

    [<Fact>]
    member _.``PUT api/products/{productCode} - 商品を更新できる`` () = task {
        // Arrange
        use client = factory.CreateClient()
        let product = TestHelpers.createTestProduct "PROD006" "テスト商品6"
        let! _ = client.PostAsync("/api/products", TestHelpers.createJsonContent product)

        let updateRequest = {| ProductFormalName = Some "更新後の商品名" |}
        let content = TestHelpers.createJsonContent updateRequest

        // Act
        let! response = client.PutAsync("/api/products/PROD006", content)

        // Assert
        response.StatusCode |> should equal HttpStatusCode.OK
        let! body = response.Content.ReadAsStringAsync()
        let result = TestHelpers.fromJson<ProductResponse>(body)
        result.ProductFormalName |> should equal "更新後の商品名"
    }

    [<Fact>]
    member _.``DELETE api/products/{productCode} - 商品を削除できる`` () = task {
        // Arrange
        use client = factory.CreateClient()
        let product = TestHelpers.createTestProduct "PROD007" "テスト商品7"
        let! _ = client.PostAsync("/api/products", TestHelpers.createJsonContent product)

        // Act
        let! response = client.DeleteAsync("/api/products/PROD007")

        // Assert
        response.StatusCode |> should equal HttpStatusCode.NoContent

        // 削除後に取得を試みて404を確認
        let! getResponse = client.GetAsync("/api/products/PROD007")
        getResponse.StatusCode |> should equal HttpStatusCode.NotFound
    }
