namespace SalesManagement.Api.Controllers

open System.Threading.Tasks
open Microsoft.AspNetCore.Mvc
open Microsoft.Extensions.Configuration
open Microsoft.Extensions.Logging
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
