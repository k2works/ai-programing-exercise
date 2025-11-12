namespace SalesManagement.Api.Controllers

open System.Threading.Tasks
open Microsoft.AspNetCore.Mvc
open Microsoft.Extensions.Configuration
open Microsoft.Extensions.Logging
open Swashbuckle.AspNetCore.Annotations
open SalesManagement.Api.Dtos
open SalesManagement.Api.Services.ProductService

[<ApiController>]
[<Route("api/products")>]
[<Produces("application/json")>]
type ProductController(configuration: IConfiguration, logger: ILogger<ProductController>) =
    inherit ControllerBase()

    let connectionString =
        configuration.GetConnectionString("DefaultConnection")
        |> Option.ofObj
        |> Option.defaultWith (fun () -> failwith "接続文字列が設定されていません")

    /// Result型からActionResultへの変換
    let toActionResult (result: Result<'T, ServiceError>) : ActionResult<'T> =
        match result with
        | Ok value -> ActionResult<'T>(value)
        | Error err ->
            match err with
            | NotFound msg ->
                ActionResult<'T>(NotFoundObjectResult({| Message = msg |}))
            | BusinessError msg ->
                ActionResult<'T>(BadRequestObjectResult({| Message = msg |}))
            | DatabaseError msg ->
                ActionResult<'T>(ObjectResult({| Message = $"データベースエラー: {msg}" |}, StatusCode = 500))

    /// <summary>
    /// 商品を作成
    /// </summary>
    [<HttpPost>]
    [<SwaggerOperation(
        Summary = "Create a new product",
        Description = """Submit a new product with all required information.

**Example Request:**
```json
{
  "productCode": "P001",
  "productFormalName": "テスト商品1",
  "productAbbreviation": "テスト1",
  "productNameKana": "テストショウヒン1",
  "productType": "TYPE001",
  "modelNumber": "MODEL001",
  "sellingPrice": 1000,
  "purchasePrice": 800,
  "costOfSales": 850,
  "taxType": 0,
  "productCategoryCode": "CAT001",
  "miscellaneousType": 0,
  "inventoryManagementFlag": 1,
  "inventoryAllocationFlag": 1,
  "supplierCode": "SUP001",
  "supplierBranch": 1
}
```

**Optional Fields (can be null):**
- modelNumber
- supplierCode
- supplierBranch"""
    )>]
    [<ProducesResponseType(typeof<ProductResponse>, 201)>]
    [<ProducesResponseType(400)>]
    member this.CreateProduct([<FromBody>] request: CreateProductRequest) : Task<IActionResult> =
        task {
            logger.LogInformation("商品作成リクエスト: {ProductCode}", request.ProductCode)
            let! result = createProductAsync connectionString request
            match result with
            | Ok response ->
                return this.CreatedAtAction("GetProductById", {| productCode = response.ProductCode |}, response) :> IActionResult
            | Error (NotFound msg) ->
                return this.NotFound({| Message = msg |}) :> IActionResult
            | Error (BusinessError msg) ->
                return this.BadRequest({| Message = msg |}) :> IActionResult
            | Error (DatabaseError msg) ->
                return this.StatusCode(500, {| Message = $"データベースエラー: {msg}" |}) :> IActionResult
        }

    /// <summary>
    /// すべての商品を取得
    /// </summary>
    [<HttpGet>]
    [<ProducesResponseType(typeof<ProductResponse list>, 200)>]
    member this.GetAllProducts() : Task<ActionResult<ProductResponse list>> =
        task {
            logger.LogInformation("商品一覧取得リクエスト")
            let! result = getAllProductsAsync connectionString
            return toActionResult result
        }

    /// <summary>
    /// ページング対応の商品一覧取得
    /// </summary>
    [<HttpGet("page")>]
    [<ProducesResponseType(typeof<PageResponse<ProductResponse>>, 200)>]
    member this.GetProducts([<FromQuery>] page: int, [<FromQuery>] size: int) : Task<ActionResult<PageResponse<ProductResponse>>> =
        task {
            let pageNum = if page < 0 then 0 else page
            let pageSize = if size <= 0 then 20 else size
            logger.LogInformation("商品一覧取得リクエスト (Page: {Page}, Size: {Size})", pageNum, pageSize)
            let! result = getProductsAsync connectionString pageNum pageSize
            return toActionResult result
        }

    /// <summary>
    /// IDで商品を取得
    /// </summary>
    [<HttpGet("{productCode}")>]
    [<ProducesResponseType(typeof<ProductResponse>, 200)>]
    [<ProducesResponseType(404)>]
    member this.GetProductById(productCode: string) : Task<ActionResult<ProductResponse>> =
        task {
            logger.LogInformation("商品取得リクエスト: {ProductCode}", productCode)
            let! result = getProductByIdAsync connectionString productCode
            return toActionResult result
        }

    /// <summary>
    /// 商品を更新
    /// </summary>
    [<HttpPut("{productCode}")>]
    [<SwaggerOperation(
        Summary = "Update an existing product",
        Description = """Update specific fields of an existing product. All fields are optional - only send the fields you want to update.

**Example Request (Update product name only):**
```json
{
  "productFormalName": "更新後の商品名"
}
```

**Example Request (Update multiple fields):**
```json
{
  "productFormalName": "更新後の商品名",
  "productAbbreviation": "更新後の略称",
  "sellingPrice": 1500,
  "purchasePrice": 1000
}
```

**Example Request (Update all fields):**
```json
{
  "productFormalName": "更新後の商品正式名",
  "productAbbreviation": "更新後略称",
  "productNameKana": "コウシンゴショウヒン",
  "productType": "TYPE002",
  "modelNumber": "MODEL002",
  "sellingPrice": 2000,
  "purchasePrice": 1500,
  "costOfSales": 1600,
  "taxType": 1,
  "productCategoryCode": "CAT002",
  "miscellaneousType": 0,
  "inventoryManagementFlag": 1,
  "inventoryAllocationFlag": 1,
  "supplierCode": "SUP002",
  "supplierBranch": 2
}
```

**All Fields (all optional):**
- productFormalName
- productAbbreviation
- productNameKana
- productType
- modelNumber
- sellingPrice
- purchasePrice
- costOfSales
- taxType
- productCategoryCode
- miscellaneousType
- inventoryManagementFlag
- inventoryAllocationFlag
- supplierCode
- supplierBranch"""
    )>]
    [<ProducesResponseType(typeof<ProductResponse>, 200)>]
    [<ProducesResponseType(404)>]
    [<ProducesResponseType(400)>]
    member this.UpdateProduct(productCode: string, [<FromBody>] request: UpdateProductRequest) : Task<ActionResult<ProductResponse>> =
        task {
            logger.LogInformation("商品更新リクエスト: {ProductCode}", productCode)
            let! result = updateProductAsync connectionString productCode request
            return toActionResult result
        }

    /// <summary>
    /// 商品を削除
    /// </summary>
    [<HttpDelete("{productCode}")>]
    [<ProducesResponseType(204)>]
    [<ProducesResponseType(404)>]
    member this.DeleteProduct(productCode: string) : Task<IActionResult> =
        task {
            logger.LogInformation("商品削除リクエスト: {ProductCode}", productCode)
            let! result = deleteProductAsync connectionString productCode
            match result with
            | Ok () -> return this.NoContent() :> IActionResult
            | Error (NotFound msg) -> return this.NotFound({| Message = msg |}) :> IActionResult
            | Error (BusinessError msg) -> return this.BadRequest({| Message = msg |}) :> IActionResult
            | Error (DatabaseError msg) -> return this.StatusCode(500, {| Message = $"データベースエラー: {msg}" |}) :> IActionResult
        }
