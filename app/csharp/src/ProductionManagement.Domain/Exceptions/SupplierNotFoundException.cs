namespace ProductionManagement.Domain.Exceptions;

/// <summary>
/// 取引先が見つからない場合の例外
/// </summary>
public class SupplierNotFoundException : DomainException
{
    public SupplierNotFoundException(string supplierCode)
        : base($"取引先が見つかりません: {supplierCode}")
    {
        SupplierCode = supplierCode;
    }

    public string SupplierCode { get; }
}
