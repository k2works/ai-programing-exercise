namespace AccountingSystem.Application.Exceptions;

/// <summary>
/// 同時実行例外
/// イベントソーシングの楽観的ロックで競合が発生した場合にスロー
/// </summary>
public class ConcurrentModificationException : Exception
{
    public ConcurrentModificationException(string message) : base(message)
    {
    }

    public ConcurrentModificationException(string message, Exception innerException)
        : base(message, innerException)
    {
    }
}
