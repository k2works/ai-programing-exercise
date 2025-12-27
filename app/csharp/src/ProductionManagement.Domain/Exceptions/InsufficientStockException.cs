namespace ProductionManagement.Domain.Exceptions;

/// <summary>
/// 在庫不足例外
/// </summary>
public class InsufficientStockException : Exception
{
    public InsufficientStockException(string message) : base(message) { }
}
