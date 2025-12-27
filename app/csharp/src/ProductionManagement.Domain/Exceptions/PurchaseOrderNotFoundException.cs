namespace ProductionManagement.Domain.Exceptions;

/// <summary>
/// 発注が見つからない例外
/// </summary>
public class PurchaseOrderNotFoundException : DomainException
{
    public string OrderNumber { get; }

    public PurchaseOrderNotFoundException(string orderNumber)
        : base($"発注が見つかりません: {orderNumber}")
    {
        OrderNumber = orderNumber;
    }
}
