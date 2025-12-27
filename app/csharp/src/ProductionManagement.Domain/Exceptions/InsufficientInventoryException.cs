namespace ProductionManagement.Domain.Exceptions;

/// <summary>
/// 在庫不足例外
/// </summary>
public class InsufficientInventoryException : DomainException
{
    public string ItemCode { get; }
    public decimal Required { get; }
    public decimal Available { get; }

    public InsufficientInventoryException(string itemCode, decimal required, decimal available)
        : base($"在庫が不足しています: {itemCode} (必要: {required}, 有効: {available})")
    {
        ItemCode = itemCode;
        Required = required;
        Available = available;
    }
}
