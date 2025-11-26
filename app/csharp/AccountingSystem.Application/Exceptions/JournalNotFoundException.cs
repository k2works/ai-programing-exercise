namespace AccountingSystem.Application.Exceptions;

/// <summary>
/// 仕訳が見つからない場合の例外
/// </summary>
public class JournalNotFoundException : Exception
{
    public JournalNotFoundException(string message) : base(message)
    {
    }
}
