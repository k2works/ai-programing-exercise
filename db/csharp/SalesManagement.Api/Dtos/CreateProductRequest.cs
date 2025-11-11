using System.ComponentModel.DataAnnotations;

namespace SalesManagement.Api.Dtos;

/// <summary>
/// 商品作成リクエストDTO
/// </summary>
public class CreateProductRequest
{
    [Required(ErrorMessage = "商品コードは必須です")]
    [StringLength(16, ErrorMessage = "商品コードは16文字以内で入力してください")]
    public string ProductCode { get; set; } = string.Empty;

    [Required(ErrorMessage = "商品正式名は必須です")]
    [StringLength(40, ErrorMessage = "商品正式名は40文字以内で入力してください")]
    public string FullName { get; set; } = string.Empty;

    [Required(ErrorMessage = "商品名は必須です")]
    [StringLength(10, ErrorMessage = "商品名は10文字以内で入力してください")]
    public string Name { get; set; } = string.Empty;

    [Required(ErrorMessage = "商品カナ名は必須です")]
    [StringLength(20, ErrorMessage = "商品カナ名は20文字以内で入力してください")]
    public string KanaName { get; set; } = string.Empty;

    [Required(ErrorMessage = "販売単価は必須です")]
    [Range(0, int.MaxValue, ErrorMessage = "販売単価は0以上で入力してください")]
    public int UnitPrice { get; set; }

    [Required(ErrorMessage = "売上原価は必須です")]
    [Range(0, int.MaxValue, ErrorMessage = "売上原価は0以上で入力してください")]
    public int PrimeCost { get; set; }

    [Required(ErrorMessage = "仕入先コードは必須です")]
    [StringLength(8, ErrorMessage = "仕入先コードは8文字以内で入力してください")]
    public string SupplierCode { get; set; } = string.Empty;
}
