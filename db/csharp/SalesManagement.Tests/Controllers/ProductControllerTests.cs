using System.Net;
using System.Net.Http.Json;
using Microsoft.AspNetCore.Mvc.Testing;
using SalesManagement.Api.Dtos;
using Xunit;
using FluentAssertions;

namespace SalesManagement.Tests.Controllers;

public class ProductControllerTests : IClassFixture<WebApplicationFactory<Program>>
{
    private readonly HttpClient _client;

    public ProductControllerTests(WebApplicationFactory<Program> factory)
    {
        _client = factory.CreateClient();
    }

    [Fact]
    public async Task CreateProduct_正常な商品を作成できる()
    {
        // Arrange
        var request = new CreateProductRequest
        {
            ProductCode = $"TEST{Guid.NewGuid():N}"[..16],
            FullName = "テスト商品",
            Name = "テスト",
            KanaName = "テストショウヒン",
            UnitPrice = 1000,
            PrimeCost = 700,
            SupplierCode = "S0000001"
        };

        // Act
        var response = await _client.PostAsJsonAsync("/api/products", request);

        // Assert
        response.StatusCode.Should().Be(HttpStatusCode.Created);
        var product = await response.Content.ReadFromJsonAsync<ProductResponse>();
        product.Should().NotBeNull();
        product!.ProductCode.Should().Be(request.ProductCode);
        product.FullName.Should().Be(request.FullName);
    }

    [Fact]
    public async Task GetAllProducts_すべての商品を取得できる()
    {
        // Act
        var response = await _client.GetAsync("/api/products");

        // Assert
        response.StatusCode.Should().Be(HttpStatusCode.OK);
        var products = await response.Content.ReadFromJsonAsync<List<ProductResponse>>();
        products.Should().NotBeNull();
    }

    [Fact]
    public async Task GetProductById_存在する商品を取得できる()
    {
        // Arrange
        var createRequest = new CreateProductRequest
        {
            ProductCode = $"TEST{Guid.NewGuid():N}"[..16],
            FullName = "テスト商品",
            Name = "テスト",
            KanaName = "テストショウヒン",
            UnitPrice = 1000,
            PrimeCost = 700,
            SupplierCode = "S0000001"
        };
        await _client.PostAsJsonAsync("/api/products", createRequest);

        // Act
        var response = await _client.GetAsync($"/api/products/{createRequest.ProductCode}");

        // Assert
        response.StatusCode.Should().Be(HttpStatusCode.OK);
        var product = await response.Content.ReadFromJsonAsync<ProductResponse>();
        product.Should().NotBeNull();
        product!.ProductCode.Should().Be(createRequest.ProductCode);
    }

    [Fact]
    public async Task GetProductById_存在しない商品で404エラー()
    {
        // Act
        var response = await _client.GetAsync("/api/products/NONEXISTENT");

        // Assert
        response.StatusCode.Should().Be(HttpStatusCode.NotFound);
    }

    [Fact]
    public async Task UpdateProduct_商品を更新できる()
    {
        // Arrange
        var createRequest = new CreateProductRequest
        {
            ProductCode = $"TEST{Guid.NewGuid():N}"[..16],
            FullName = "テスト商品",
            Name = "テスト",
            KanaName = "テストショウヒン",
            UnitPrice = 1000,
            PrimeCost = 700,
            SupplierCode = "S0000001"
        };
        await _client.PostAsJsonAsync("/api/products", createRequest);

        var updateRequest = new UpdateProductRequest
        {
            FullName = "更新後の商品名",
            UnitPrice = 1200
        };

        // Act
        var response = await _client.PutAsJsonAsync(
            $"/api/products/{createRequest.ProductCode}", updateRequest);

        // Assert
        response.StatusCode.Should().Be(HttpStatusCode.OK);
        var product = await response.Content.ReadFromJsonAsync<ProductResponse>();
        product.Should().NotBeNull();
        product!.FullName.Should().Be("更新後の商品名");
        product.UnitPrice.Should().Be(1200);
    }

    [Fact]
    public async Task DeleteProduct_商品を削除できる()
    {
        // Arrange
        var createRequest = new CreateProductRequest
        {
            ProductCode = $"TEST{Guid.NewGuid():N}"[..16],
            FullName = "テスト商品",
            Name = "テスト",
            KanaName = "テストショウヒン",
            UnitPrice = 1000,
            PrimeCost = 700,
            SupplierCode = "S0000001"
        };
        await _client.PostAsJsonAsync("/api/products", createRequest);

        // Act
        var response = await _client.DeleteAsync($"/api/products/{createRequest.ProductCode}");

        // Assert
        response.StatusCode.Should().Be(HttpStatusCode.NoContent);

        // 削除後に取得すると404
        var getResponse = await _client.GetAsync($"/api/products/{createRequest.ProductCode}");
        getResponse.StatusCode.Should().Be(HttpStatusCode.NotFound);
    }

    [Fact]
    public async Task CreateProduct_バリデーションエラーで400エラー()
    {
        // Arrange
        var request = new CreateProductRequest
        {
            ProductCode = "", // 必須フィールドが空
            FullName = "",
            Name = "",
            KanaName = "",
            UnitPrice = -100, // 負の値
            PrimeCost = 0,
            SupplierCode = ""
        };

        // Act
        var response = await _client.PostAsJsonAsync("/api/products", request);

        // Assert
        response.StatusCode.Should().Be(HttpStatusCode.BadRequest);
    }
}
