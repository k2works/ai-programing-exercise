namespace AccountingSystem.Application.Exceptions;

/// <summary>
/// 仕訳が無効な例外
/// </summary>
public class InvalidJournalEntryException : Exception
{
    public InvalidJournalEntryException(string message) : base(message)
    {
    }

    public InvalidJournalEntryException(string message, Exception innerException) : base(message, innerException)
    {
    }
}
