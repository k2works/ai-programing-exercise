using System.ComponentModel.DataAnnotations;
using ProductionManagement.Domain.Models.Supplier;

namespace ProductionManagement.Infrastructure.Rest.Dto;

/// <summary>
/// 取引先登録リクエスト
/// </summary>
public record CreateSupplierRequest(
    [Required(ErrorMessage = "取引先コードは必須です")]
    [StringLength(20, ErrorMessage = "取引先コードは20文字以内で入力してください")]
    string SupplierCode,

    [Required(ErrorMessage = "取引先名は必須です")]
    [StringLength(100, ErrorMessage = "取引先名は100文字以内で入力してください")]
    string SupplierName,

    [Required(ErrorMessage = "取引先区分は必須です")]
    string SupplierType,

    string? SupplierNameKana = null,
    string? PostalCode = null,
    string? Address = null,
    string? PhoneNumber = null,
    string? FaxNumber = null,
    string? ContactPerson = null
);

/// <summary>
/// 取引先レスポンス
/// </summary>
public record SupplierResponse(
    string SupplierCode,
    DateOnly EffectiveFrom,
    DateOnly? EffectiveTo,
    string SupplierName,
    string? SupplierNameKana,
    string SupplierType,
    string? PostalCode,
    string? Address,
    string? PhoneNumber,
    string? FaxNumber,
    string? ContactPerson
)
{
    public static SupplierResponse From(Supplier supplier) => new(
        SupplierCode: supplier.SupplierCode,
        EffectiveFrom: supplier.EffectiveFrom,
        EffectiveTo: supplier.EffectiveTo,
        SupplierName: supplier.SupplierName,
        SupplierNameKana: supplier.SupplierNameKana,
        SupplierType: supplier.SupplierType.GetDisplayName(),
        PostalCode: supplier.PostalCode,
        Address: supplier.Address,
        PhoneNumber: supplier.PhoneNumber,
        FaxNumber: supplier.FaxNumber,
        ContactPerson: supplier.ContactPerson
    );
}
