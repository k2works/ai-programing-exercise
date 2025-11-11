using Microsoft.AspNetCore.Mvc;
using SalesManagement.Api.Dtos;
using SalesManagement.Api.Services;

namespace SalesManagement.Api.Controllers;

[ApiController]
[Route("api/products")]
[Produces("application/json")]
public class ProductController : ControllerBase
{
    private readonly ProductService _productService;
    private readonly ILogger<ProductController> _logger;

    public ProductController(ProductService productService, ILogger<ProductController> logger)
    {
        _productService = productService;
        _logger = logger;
    }

    /// <summary>
    /// 商品を作成
    /// </summary>
    /// <param name="request">商品作成リクエスト</param>
    /// <returns>作成された商品</returns>
    [HttpPost]
    [ProducesResponseType(typeof(ProductResponse), StatusCodes.Status201Created)]
    [ProducesResponseType(StatusCodes.Status400BadRequest)]
    public async Task<ActionResult<ProductResponse>> CreateProduct(
        [FromBody] CreateProductRequest request)
    {
        _logger.LogInformation("商品作成リクエスト: {ProductCode}", request.ProductCode);
        var response = await _productService.CreateProductAsync(request);
        return CreatedAtAction(nameof(GetProductById),
            new { productCode = response.ProductCode }, response);
    }

    /// <summary>
    /// すべての商品を取得
    /// </summary>
    /// <returns>商品一覧</returns>
    [HttpGet]
    [ProducesResponseType(typeof(List<ProductResponse>), StatusCodes.Status200OK)]
    public async Task<ActionResult<List<ProductResponse>>> GetAllProducts()
    {
        _logger.LogInformation("商品一覧取得リクエスト");
        var products = await _productService.GetAllProductsAsync();
        return Ok(products);
    }

    /// <summary>
    /// ページング対応の商品一覧取得
    /// </summary>
    /// <param name="page">ページ番号（0から開始）</param>
    /// <param name="size">1ページあたりの件数</param>
    /// <returns>ページング付き商品一覧</returns>
    [HttpGet("page")]
    [ProducesResponseType(typeof(PageResponse<ProductResponse>), StatusCodes.Status200OK)]
    public async Task<ActionResult<PageResponse<ProductResponse>>> GetProducts(
        [FromQuery] int page = 0,
        [FromQuery] int size = 20)
    {
        _logger.LogInformation("商品一覧取得リクエスト (Page: {Page}, Size: {Size})", page, size);
        var response = await _productService.GetProductsAsync(page, size);
        return Ok(response);
    }

    /// <summary>
    /// IDで商品を取得
    /// </summary>
    /// <param name="productCode">商品コード</param>
    /// <returns>商品詳細</returns>
    [HttpGet("{productCode}")]
    [ProducesResponseType(typeof(ProductResponse), StatusCodes.Status200OK)]
    [ProducesResponseType(StatusCodes.Status404NotFound)]
    public async Task<ActionResult<ProductResponse>> GetProductById(string productCode)
    {
        _logger.LogInformation("商品取得リクエスト: {ProductCode}", productCode);
        var response = await _productService.GetProductByIdAsync(productCode);
        return Ok(response);
    }

    /// <summary>
    /// 商品を更新
    /// </summary>
    /// <param name="productCode">商品コード</param>
    /// <param name="request">商品更新リクエスト</param>
    /// <returns>更新された商品</returns>
    [HttpPut("{productCode}")]
    [ProducesResponseType(typeof(ProductResponse), StatusCodes.Status200OK)]
    [ProducesResponseType(StatusCodes.Status404NotFound)]
    [ProducesResponseType(StatusCodes.Status400BadRequest)]
    public async Task<ActionResult<ProductResponse>> UpdateProduct(
        string productCode,
        [FromBody] UpdateProductRequest request)
    {
        _logger.LogInformation("商品更新リクエスト: {ProductCode}", productCode);
        var response = await _productService.UpdateProductAsync(productCode, request);
        return Ok(response);
    }

    /// <summary>
    /// 商品を削除
    /// </summary>
    /// <param name="productCode">商品コード</param>
    [HttpDelete("{productCode}")]
    [ProducesResponseType(StatusCodes.Status204NoContent)]
    [ProducesResponseType(StatusCodes.Status404NotFound)]
    public async Task<IActionResult> DeleteProduct(string productCode)
    {
        _logger.LogInformation("商品削除リクエスト: {ProductCode}", productCode);
        await _productService.DeleteProductAsync(productCode);
        return NoContent();
    }
}
