namespace ProductionManagement.Domain.Exceptions;

/// <summary>
/// オーダが見つからない場合の例外
/// </summary>
public class OrderNotFoundException : DomainException
{
    public OrderNotFoundException(int orderId)
        : base($"オーダが見つかりません: ID={orderId}")
    {
        OrderId = orderId;
    }

    public OrderNotFoundException(string orderNumber)
        : base($"オーダが見つかりません: {orderNumber}")
    {
        OrderNumber = orderNumber;
    }

    public int? OrderId { get; }
    public string? OrderNumber { get; }
}
