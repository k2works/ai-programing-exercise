namespace ProductionManagement.Domain.Exceptions;

/// <summary>
/// 品目が見つからない例外
/// </summary>
public class ItemNotFoundException : DomainException
{
    public string ItemCode { get; }

    public ItemNotFoundException(string itemCode)
        : base($"品目が見つかりません: {itemCode}")
    {
        ItemCode = itemCode;
    }
}
