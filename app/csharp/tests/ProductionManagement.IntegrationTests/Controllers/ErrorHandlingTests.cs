using System.Net;
using System.Net.Http.Json;
using FluentAssertions;
using Microsoft.AspNetCore.Mvc;
using ProductionManagement.Infrastructure.Rest.Dto;
using ProductionManagement.IntegrationTests.TestSetup;

namespace ProductionManagement.IntegrationTests.Controllers;

/// <summary>
/// エラーハンドリングの統合テスト
/// </summary>
[Collection("Postgres")]
public class ErrorHandlingTests : IAsyncLifetime
{
    private readonly PostgresFixture _fixture;
    private readonly CustomWebApplicationFactory _factory;
    private readonly HttpClient _client;

    public ErrorHandlingTests(PostgresFixture fixture)
    {
        _fixture = fixture;
        _factory = new CustomWebApplicationFactory(fixture.ConnectionString);
        _client = _factory.CreateClient();
    }

    public Task InitializeAsync() => _fixture.CleanDatabaseAsync();

    public Task DisposeAsync()
    {
        _client.Dispose();
        _factory.Dispose();
        return Task.CompletedTask;
    }

    [Fact]
    public async Task NotFoundError_ReturnsProblemDetails()
    {
        // Act
        var response = await _client.GetAsync("/api/items/NOT-EXIST");

        // Assert
        response.StatusCode.Should().Be(HttpStatusCode.NotFound);
        response.Content.Headers.ContentType?.MediaType.Should().Be("application/problem+json");

        var problem = await response.Content.ReadFromJsonAsync<ProblemDetails>();
        problem.Should().NotBeNull();
        problem!.Status.Should().Be(404);
        problem.Title.Should().Be("品目が見つかりません");
        problem.Type.Should().Be("https://api.example.com/errors/item-not-found");
    }

    [Fact]
    public async Task DuplicateError_ReturnsConflictWithProblemDetails()
    {
        // Arrange
        var createRequest = new CreateItemRequest(
            ItemCode: "DUP-001",
            ItemName: "重複テスト品目",
            Category: "Product");

        await _client.PostAsJsonAsync("/api/items", createRequest);

        // Act - 同じ品目コードで再度作成
        var response = await _client.PostAsJsonAsync("/api/items", createRequest);

        // Assert
        response.StatusCode.Should().Be(HttpStatusCode.Conflict);
        response.Content.Headers.ContentType?.MediaType.Should().Be("application/problem+json");

        var problem = await response.Content.ReadFromJsonAsync<ProblemDetails>();
        problem.Should().NotBeNull();
        problem!.Status.Should().Be(409);
        problem.Title.Should().Be("品目コード重複");
        problem.Type.Should().Be("https://api.example.com/errors/duplicate-item");
    }

    [Fact]
    public async Task SupplierNotFound_ReturnsProblemDetails()
    {
        // Act
        var response = await _client.GetAsync("/api/suppliers/NOT-EXIST");

        // Assert
        response.StatusCode.Should().Be(HttpStatusCode.NotFound);
        response.Content.Headers.ContentType?.MediaType.Should().Be("application/problem+json");

        var problem = await response.Content.ReadFromJsonAsync<ProblemDetails>();
        problem.Should().NotBeNull();
        problem!.Status.Should().Be(404);
        problem.Title.Should().Be("取引先が見つかりません");
        problem.Type.Should().Be("https://api.example.com/errors/supplier-not-found");
    }

    [Fact]
    public async Task DuplicateSupplier_ReturnsConflictWithProblemDetails()
    {
        // Arrange
        var createRequest = new CreateSupplierRequest(
            SupplierCode: "SUP-DUP",
            SupplierName: "重複テスト取引先",
            SupplierType: "仕入先");

        await _client.PostAsJsonAsync("/api/suppliers", createRequest);

        // Act - 同じ取引先コードで再度作成
        var response = await _client.PostAsJsonAsync("/api/suppliers", createRequest);

        // Assert
        response.StatusCode.Should().Be(HttpStatusCode.Conflict);
        response.Content.Headers.ContentType?.MediaType.Should().Be("application/problem+json");

        var problem = await response.Content.ReadFromJsonAsync<ProblemDetails>();
        problem.Should().NotBeNull();
        problem!.Status.Should().Be(409);
        problem.Title.Should().Be("取引先コード重複");
        problem.Type.Should().Be("https://api.example.com/errors/duplicate-supplier");
    }

    [Fact]
    public async Task OrderNotFound_ReturnsProblemDetails()
    {
        // Act
        var response = await _client.GetAsync("/api/orders/99999");

        // Assert
        response.StatusCode.Should().Be(HttpStatusCode.NotFound);
        response.Content.Headers.ContentType?.MediaType.Should().Be("application/problem+json");

        var problem = await response.Content.ReadFromJsonAsync<ProblemDetails>();
        problem.Should().NotBeNull();
        problem!.Status.Should().Be(404);
        problem.Title.Should().Be("オーダが見つかりません");
        problem.Type.Should().Be("https://api.example.com/errors/order-not-found");
    }

    [Fact]
    public async Task ProblemDetails_ContainsTimestamp()
    {
        // Act
        var response = await _client.GetAsync("/api/items/NOT-EXIST");

        // Assert
        var problem = await response.Content.ReadFromJsonAsync<ProblemDetails>();
        problem.Should().NotBeNull();
        problem!.Extensions.Should().ContainKey("timestamp");
    }

    [Fact]
    public async Task ProblemDetails_ContainsInstance()
    {
        // Act
        var response = await _client.GetAsync("/api/items/NOT-EXIST");

        // Assert
        var problem = await response.Content.ReadFromJsonAsync<ProblemDetails>();
        problem.Should().NotBeNull();
        problem!.Instance.Should().Be("/api/items/NOT-EXIST");
    }
}
