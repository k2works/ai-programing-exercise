using System.ComponentModel.DataAnnotations;
using SalesManagement.Api.Dtos;
using Xunit;
using FluentAssertions;

namespace SalesManagement.Tests.Dtos;

public class ProductDtoTests
{
    [Fact]
    public void CreateProductRequest_正常な商品作成リクエストを検証できる()
    {
        // Arrange
        var request = new CreateProductRequest
        {
            ProductCode = "PROD00001",
            FullName = "黒毛和牛サーロインステーキ 200g",
            Name = "サーロイン",
            KanaName = "クロゲワギュウサーロイン",
            UnitPrice = 5000,
            PrimeCost = 3500,
            SupplierCode = "SUP00001"
        };

        // Act
        var validationResults = ValidateModel(request);

        // Assert
        validationResults.Should().BeEmpty();
    }

    [Fact]
    public void CreateProductRequest_負の単価を拒否する()
    {
        // Arrange
        var request = new CreateProductRequest
        {
            ProductCode = "PROD00001",
            FullName = "黒毛和牛サーロインステーキ 200g",
            Name = "サーロイン",
            KanaName = "クロゲワギュウサーロイン",
            UnitPrice = -100, // 負の値
            PrimeCost = 3500,
            SupplierCode = "SUP00001"
        };

        // Act
        var validationResults = ValidateModel(request);

        // Assert
        validationResults.Should().NotBeEmpty();
        validationResults.Should().Contain(v => v.MemberNames.Contains("UnitPrice"));
    }

    [Fact]
    public void CreateProductRequest_必須フィールド欠落を拒否する()
    {
        // Arrange
        var request = new CreateProductRequest
        {
            ProductCode = "PROD00001",
            // FullName が欠落
            Name = "サーロイン",
            KanaName = "クロゲワギュウサーロイン",
            UnitPrice = 5000,
            PrimeCost = 3500,
            SupplierCode = "SUP00001"
        };

        // Act
        var validationResults = ValidateModel(request);

        // Assert
        validationResults.Should().NotBeEmpty();
        validationResults.Should().Contain(v => v.MemberNames.Contains("FullName"));
    }

    [Fact]
    public void UpdateProductRequest_正常な商品更新リクエストを検証できる()
    {
        // Arrange
        var request = new UpdateProductRequest
        {
            FullName = "黒毛和牛サーロインステーキ 250g",
            UnitPrice = 5500
        };

        // Act
        var validationResults = ValidateModel(request);

        // Assert
        validationResults.Should().BeEmpty();
    }

    [Fact]
    public void UpdateProductRequest_空のオブジェクトを許可する()
    {
        // Arrange
        var request = new UpdateProductRequest();

        // Act
        var validationResults = ValidateModel(request);

        // Assert
        validationResults.Should().BeEmpty();
    }

    private static List<ValidationResult> ValidateModel(object model)
    {
        var validationResults = new List<ValidationResult>();
        var validationContext = new ValidationContext(model);
        Validator.TryValidateObject(model, validationContext, validationResults, true);
        return validationResults;
    }
}
