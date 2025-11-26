namespace AccountingSystem.Application.Exceptions;

/// <summary>
/// 勘定科目が重複している例外
/// </summary>
public class DuplicateAccountException : Exception
{
    public DuplicateAccountException(string message) : base(message)
    {
    }

    public DuplicateAccountException(string message, Exception innerException) : base(message, innerException)
    {
    }
}
