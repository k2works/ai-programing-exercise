using System.ComponentModel.DataAnnotations;
using Microsoft.AspNetCore.Mvc.Rendering;
using ProductionManagement.Domain.Models.Supplier;

namespace ProductionManagement.Web.Models;

/// <summary>
/// 取引先一覧 ViewModel
/// </summary>
public class SupplierListViewModel
{
    public List<SupplierViewModel> Suppliers { get; set; } = [];
    public SupplierSearchViewModel Search { get; set; } = new();
}

/// <summary>
/// 取引先検索 ViewModel
/// </summary>
public class SupplierSearchViewModel
{
    [Display(Name = "キーワード")]
    public string? Keyword { get; set; }

    [Display(Name = "取引先区分")]
    public string? SupplierType { get; set; }

    public List<SelectListItem> SupplierTypeOptions { get; set; } = [];
}

/// <summary>
/// 取引先 ViewModel
/// </summary>
public class SupplierViewModel
{
    public int Id { get; set; }

    [Display(Name = "取引先コード")]
    [Required(ErrorMessage = "取引先コードは必須です")]
    [StringLength(20, ErrorMessage = "取引先コードは20文字以内で入力してください")]
    public string? SupplierCode { get; set; }

    [Display(Name = "取引先名")]
    [Required(ErrorMessage = "取引先名は必須です")]
    [StringLength(100, ErrorMessage = "取引先名は100文字以内で入力してください")]
    public string? SupplierName { get; set; }

    [Display(Name = "取引先名（カナ）")]
    [StringLength(100, ErrorMessage = "取引先名（カナ）は100文字以内で入力してください")]
    public string? SupplierNameKana { get; set; }

    [Display(Name = "取引先区分")]
    public SupplierType SupplierType { get; set; } = SupplierType.Vendor;

    [Display(Name = "取引先区分")]
    public string SupplierTypeDisplayName => SupplierType.GetDisplayName();

    [Display(Name = "郵便番号")]
    [StringLength(10, ErrorMessage = "郵便番号は10文字以内で入力してください")]
    public string? PostalCode { get; set; }

    [Display(Name = "住所")]
    [StringLength(200, ErrorMessage = "住所は200文字以内で入力してください")]
    public string? Address { get; set; }

    [Display(Name = "電話番号")]
    [StringLength(20, ErrorMessage = "電話番号は20文字以内で入力してください")]
    public string? PhoneNumber { get; set; }

    [Display(Name = "FAX番号")]
    [StringLength(20, ErrorMessage = "FAX番号は20文字以内で入力してください")]
    public string? FaxNumber { get; set; }

    [Display(Name = "担当者")]
    [StringLength(50, ErrorMessage = "担当者は50文字以内で入力してください")]
    public string? ContactPerson { get; set; }

    [Display(Name = "有効開始日")]
    public DateOnly EffectiveFrom { get; set; }

    [Display(Name = "有効終了日")]
    public DateOnly? EffectiveTo { get; set; }

    [Display(Name = "作成日時")]
    public DateTime CreatedAt { get; set; }

    [Display(Name = "更新日時")]
    public DateTime UpdatedAt { get; set; }

    /// <summary>
    /// ドメインモデルから ViewModel を生成
    /// </summary>
    public static SupplierViewModel FromDomain(Supplier supplier)
    {
        return new SupplierViewModel
        {
            SupplierCode = supplier.SupplierCode,
            SupplierName = supplier.SupplierName,
            SupplierNameKana = supplier.SupplierNameKana,
            SupplierType = supplier.SupplierType,
            PostalCode = supplier.PostalCode,
            Address = supplier.Address,
            PhoneNumber = supplier.PhoneNumber,
            FaxNumber = supplier.FaxNumber,
            ContactPerson = supplier.ContactPerson,
            EffectiveFrom = supplier.EffectiveFrom,
            EffectiveTo = supplier.EffectiveTo,
            CreatedAt = supplier.CreatedAt,
            UpdatedAt = supplier.UpdatedAt
        };
    }
}
