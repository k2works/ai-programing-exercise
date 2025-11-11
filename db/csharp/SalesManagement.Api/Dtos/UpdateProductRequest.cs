using System.ComponentModel.DataAnnotations;

namespace SalesManagement.Api.Dtos;

/// <summary>
/// 商品更新リクエストDTO
/// すべてのフィールドはオプション
/// </summary>
public class UpdateProductRequest
{
    [StringLength(40, ErrorMessage = "商品正式名は40文字以内で入力してください")]
    public string? FullName { get; set; }

    [StringLength(10, ErrorMessage = "商品名は10文字以内で入力してください")]
    public string? Name { get; set; }

    [StringLength(20, ErrorMessage = "商品カナ名は20文字以内で入力してください")]
    public string? KanaName { get; set; }

    [Range(0, int.MaxValue, ErrorMessage = "販売単価は0以上で入力してください")]
    public int? UnitPrice { get; set; }

    [Range(0, int.MaxValue, ErrorMessage = "売上原価は0以上で入力してください")]
    public int? PrimeCost { get; set; }

    [StringLength(8, ErrorMessage = "仕入先コードは8文字以内で入力してください")]
    public string? SupplierCode { get; set; }
}
