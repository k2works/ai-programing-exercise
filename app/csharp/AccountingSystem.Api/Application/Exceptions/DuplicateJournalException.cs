namespace AccountingSystem.Application.Exceptions;

/// <summary>
/// 仕訳が重複している場合の例外
/// </summary>
public class DuplicateJournalException : Exception
{
    public DuplicateJournalException(string message) : base(message)
    {
    }
}
