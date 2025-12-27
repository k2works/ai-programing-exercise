namespace ProductionManagement.Domain.Exceptions;

/// <summary>
/// 品目コード重複例外
/// </summary>
public class DuplicateItemException : DomainException
{
    public string ItemCode { get; }

    public DuplicateItemException(string itemCode)
        : base($"品目コードが既に存在します: {itemCode}")
    {
        ItemCode = itemCode;
    }
}
