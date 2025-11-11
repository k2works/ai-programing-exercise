using SalesManagement.Domain.Models;

namespace SalesManagement.Api.Dtos;

/// <summary>
/// 商品レスポンスDTO
/// </summary>
public class ProductResponse
{
    public string ProductCode { get; set; } = string.Empty;
    public string FullName { get; set; } = string.Empty;
    public string Name { get; set; } = string.Empty;
    public string KanaName { get; set; } = string.Empty;
    public int UnitPrice { get; set; }
    public int PrimeCost { get; set; }
    public string SupplierCode { get; set; } = string.Empty;
    public DateTime CreatedAt { get; set; }
    public string CreatedBy { get; set; } = string.Empty;
    public DateTime UpdatedAt { get; set; }
    public string UpdatedBy { get; set; } = string.Empty;

    /// <summary>
    /// EntityからDTOへの変換
    /// </summary>
    public static ProductResponse FromEntity(Product product)
    {
        return new ProductResponse
        {
            ProductCode = product.ProductCode,
            FullName = product.ProductFormalName,
            Name = product.ProductAbbreviation,
            KanaName = product.ProductNameKana,
            UnitPrice = product.SellingPrice,
            PrimeCost = product.CostOfSales,
            SupplierCode = product.SupplierCode ?? string.Empty,
            CreatedAt = product.CreatedAt,
            CreatedBy = product.CreatedBy,
            UpdatedAt = product.UpdatedAt,
            UpdatedBy = product.UpdatedBy
        };
    }
}
