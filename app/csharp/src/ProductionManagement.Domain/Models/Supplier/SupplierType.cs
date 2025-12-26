namespace ProductionManagement.Domain.Models.Supplier;

/// <summary>
/// 取引先区分
/// </summary>
public enum SupplierType
{
    /// <summary>仕入先</summary>
    Vendor,

    /// <summary>外注先</summary>
    Subcontractor,

    /// <summary>得意先</summary>
    Customer,

    /// <summary>仕入先兼外注先</summary>
    VendorAndSubcontractor
}

/// <summary>
/// SupplierType 拡張メソッド
/// </summary>
public static class SupplierTypeExtensions
{
    private static readonly Dictionary<SupplierType, string> DisplayNames = new()
    {
        { SupplierType.Vendor, "仕入先" },
        { SupplierType.Subcontractor, "外注先" },
        { SupplierType.Customer, "得意先" },
        { SupplierType.VendorAndSubcontractor, "仕入先兼外注先" }
    };

    private static readonly Dictionary<string, SupplierType> FromDisplayNames =
        DisplayNames.ToDictionary(x => x.Value, x => x.Key);

    public static string GetDisplayName(this SupplierType supplierType)
    {
        return DisplayNames[supplierType];
    }

    public static SupplierType FromDisplayName(string displayName)
    {
        if (FromDisplayNames.TryGetValue(displayName, out var supplierType))
        {
            return supplierType;
        }

        throw new ArgumentException($"不正な取引先区分: {displayName}");
    }
}
