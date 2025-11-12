module ProductControllerTests

open System
open System.Net
open System.Net.Http
open System.Text
open System.Text.Json
open System.Text.Json.Serialization
open Xunit
open FsUnit.Xunit
open Microsoft.AspNetCore.Mvc.Testing
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.Configuration
open Npgsql
open Dapper
open SalesManagement.Api
open SalesManagement.Api.Dtos
open SalesManagement.Tests.DatabaseTestBase

/// テスト用の WebApplicationFactory
type ApiTestFactory(connectionString: string) =
    inherit WebApplicationFactory<Program>()

    override _.ConfigureWebHost(builder: IWebHostBuilder) =
        // testcontainers の接続文字列で上書き
        builder.ConfigureAppConfiguration(Action<WebHostBuilderContext, IConfigurationBuilder>(fun _ config ->
            config.AddInMemoryCollection(
                dict [
                    "ConnectionStrings:DefaultConnection", connectionString
                ]
            ) |> ignore
        )) |> ignore

/// テスト用のヘルパー関数
module TestHelpers =
    /// データベースをクリーンアップ
    let cleanupDatabase (connectionString: string) = task {
        use connection = new NpgsqlConnection(connectionString)
        do! connection.ExecuteAsync("DELETE FROM 商品マスタ") |> Async.AwaitTask |> Async.Ignore
    }

    /// テスト用マスターデータをセットアップ
    let setupMasterData (connectionString: string) = task {
        use connection = new NpgsqlConnection(connectionString)
        let now = DateTime.Now

        // 商品分類を登録 (必須のマスターデータ)
        let! _ = connection.ExecuteAsync(
            "INSERT INTO 商品分類マスタ (商品分類コード, 商品分類名, 商品分類階層, 商品分類パス, 最下層区分, 作成日時, 作成者名, 更新日時, 更新者名) VALUES (@ProductCategoryCode, @ProductCategoryName, @ProductCategoryLevel, @ProductCategoryPath, @LowestLevelFlag, @CreatedAt, @CreatedBy, @UpdatedAt, @UpdatedBy) ON CONFLICT (商品分類コード) DO NOTHING",
            {| ProductCategoryCode = "CAT001"; ProductCategoryName = "テスト分類"; ProductCategoryLevel = 1; ProductCategoryPath = "CAT001"; LowestLevelFlag = 1; CreatedAt = now; CreatedBy = "test"; UpdatedAt = now; UpdatedBy = "test" |})

        return ()
    }

    /// テスト用商品データを作成
    let createTestProduct code formalName =
        {| ProductCode = code
           ProductFormalName = formalName
           ProductAbbreviation = "略称"
           ProductNameKana = "カナ"
           ProductType = "TYPE001"
           ModelNumber = None
           SellingPrice = 1000
           PurchasePrice = 800
           CostOfSales = 850
           TaxType = 0
           ProductCategoryCode = "CAT001"
           MiscellaneousType = 0
           InventoryManagementFlag = 1
           InventoryAllocationFlag = 1
           SupplierCode = None
           SupplierBranch = None |}

    /// JSON にシリアライズ
    let toJson obj =
        let options = JsonSerializerOptions()
        options.PropertyNamingPolicy <- JsonNamingPolicy.CamelCase
        options.Converters.Add(JsonFSharpConverter())
        JsonSerializer.Serialize(obj, options)

    /// JSON から デシリアライズ
    let fromJson<'T> (json: string) =
        let options = JsonSerializerOptions()
        options.PropertyNamingPolicy <- JsonNamingPolicy.CamelCase
        options.Converters.Add(JsonFSharpConverter())
        JsonSerializer.Deserialize<'T>(json, options)

    /// HttpContent を作成
    let createJsonContent obj =
        new StringContent(toJson obj, Encoding.UTF8, "application/json")

/// 統合テスト
[<Collection("ProductController Tests")>]
type ProductControllerIntegrationTests(db: DatabaseTestBase) =
    // テスト前にデータベースをクリーンアップし、マスターデータをセットアップ
    do
        TestHelpers.cleanupDatabase(db.ConnectionString) |> Async.AwaitTask |> Async.RunSynchronously
        TestHelpers.setupMasterData(db.ConnectionString) |> Async.AwaitTask |> Async.RunSynchronously

    interface IClassFixture<DatabaseTestBase>

    [<Fact>]
    member _.``POST api/products - 商品を作成できる`` () = task {
        // Arrange
        use factory = new ApiTestFactory(db.ConnectionString)
        use client = factory.CreateClient()
        let product = TestHelpers.createTestProduct "PROD001" "テスト商品1"
        let content = TestHelpers.createJsonContent product

        // Act
        let! response = client.PostAsync("/api/products", content)
        let! body = response.Content.ReadAsStringAsync()

        // Debug: エラー時にレスポンスを出力
        if not response.IsSuccessStatusCode then
            printfn "Response Status: %A" response.StatusCode
            printfn "Response Body: %s" body

        // Assert
        response.StatusCode |> should equal HttpStatusCode.Created
        let result = TestHelpers.fromJson<ProductResponse>(body)
        result.ProductCode |> should equal "PROD001"
        result.ProductFormalName |> should equal "テスト商品1"
    }

    [<Fact>]
    member _.``POST api/products - 重複する商品コードでエラー`` () = task {
        // Arrange
        use factory = new ApiTestFactory(db.ConnectionString)
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
        use factory = new ApiTestFactory(db.ConnectionString)
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
        use factory = new ApiTestFactory(db.ConnectionString)
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
        use factory = new ApiTestFactory(db.ConnectionString)
        use client = factory.CreateClient()

        // Act
        let! response = client.GetAsync("/api/products/NOTFOUND")

        // Assert
        response.StatusCode |> should equal HttpStatusCode.NotFound
    }

    [<Fact>]
    member _.``PUT api/products/{productCode} - 商品を更新できる`` () = task {
        // Arrange
        use factory = new ApiTestFactory(db.ConnectionString)
        use client = factory.CreateClient()
        let product = TestHelpers.createTestProduct "PROD006" "テスト商品6"
        let productJson = TestHelpers.toJson product
        printfn "POST JSON: %s" productJson

        let! postResponse = client.PostAsync("/api/products", TestHelpers.createJsonContent product)

        // POST が成功したことを確認
        if not postResponse.IsSuccessStatusCode then
            let! postBody = postResponse.Content.ReadAsStringAsync()
            printfn "POST failed - Status: %A, Body: %s" postResponse.StatusCode postBody

        postResponse.StatusCode |> should equal HttpStatusCode.Created

        // UpdateProductRequest 型を使用（すべてのフィールドを明示的に指定）
        let updateRequest : SalesManagement.Api.Dtos.UpdateProductRequest =
            { ProductFormalName = Some "更新後の商品名"
              ProductAbbreviation = None
              ProductNameKana = None
              ProductType = None
              ModelNumber = None
              SellingPrice = None
              PurchasePrice = None
              CostOfSales = None
              TaxType = None
              ProductCategoryCode = None
              MiscellaneousType = None
              InventoryManagementFlag = None
              InventoryAllocationFlag = None
              SupplierCode = None
              SupplierBranch = None }

        let jsonBody = TestHelpers.toJson updateRequest
        printfn "Request JSON: %s" jsonBody

        let content = TestHelpers.createJsonContent updateRequest

        // Act
        let! response = client.PutAsync("/api/products/PROD006", content)
        let! body = response.Content.ReadAsStringAsync()

        // Debug: エラー時にレスポンスを出力
        if not response.IsSuccessStatusCode then
            printfn "Response Status: %A" response.StatusCode
            printfn "Response Body: %s" body

        // Assert
        response.StatusCode |> should equal HttpStatusCode.OK
        let result = TestHelpers.fromJson<ProductResponse>(body)
        result.ProductFormalName |> should equal "更新後の商品名"
    }

    [<Fact>]
    member _.``DELETE api/products/{productCode} - 商品を削除できる`` () = task {
        // Arrange
        use factory = new ApiTestFactory(db.ConnectionString)
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
