namespace ProductionManagement.Domain.Models.Supplier;

/// <summary>
/// 取引先マスタ
/// </summary>
public class Supplier
{
    private string _supplierTypeValue = "仕入先";

    public required string SupplierCode { get; set; }

    public DateOnly EffectiveFrom { get; set; }

    public DateOnly? EffectiveTo { get; set; }

    public required string SupplierName { get; set; }

    public string? SupplierNameKana { get; set; }

    /// <summary>
    /// 取引先区分（データベース格納用の日本語文字列）
    /// </summary>
    public string SupplierTypeValue
    {
        get => _supplierTypeValue;
        set => _supplierTypeValue = value;
    }

    /// <summary>
    /// 取引先区分（ドメインロジック用の Enum）
    /// </summary>
    public SupplierType SupplierType
    {
        get => SupplierTypeExtensions.FromDisplayName(_supplierTypeValue);
        set => _supplierTypeValue = value.GetDisplayName();
    }

    public string? PostalCode { get; set; }

    public string? Address { get; set; }

    public string? PhoneNumber { get; set; }

    public string? FaxNumber { get; set; }

    public string? ContactPerson { get; set; }

    public DateTime CreatedAt { get; set; }

    public DateTime UpdatedAt { get; set; }
}
