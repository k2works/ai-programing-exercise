namespace AccountingSystem.Application.Exceptions;

/// <summary>
/// 勘定科目が見つからない例外
/// </summary>
public class AccountNotFoundException : Exception
{
    public AccountNotFoundException(string message) : base(message)
    {
    }

    public AccountNotFoundException(string message, Exception innerException) : base(message, innerException)
    {
    }
}
