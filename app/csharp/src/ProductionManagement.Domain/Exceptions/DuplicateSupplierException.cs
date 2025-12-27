namespace ProductionManagement.Domain.Exceptions;

/// <summary>
/// 取引先が重複している場合の例外
/// </summary>
public class DuplicateSupplierException : DomainException
{
    public DuplicateSupplierException(string supplierCode)
        : base($"取引先が既に存在します: {supplierCode}")
    {
        SupplierCode = supplierCode;
    }

    public string SupplierCode { get; }
}
