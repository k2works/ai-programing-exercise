namespace ProductionManagement.Domain.Exceptions;

/// <summary>
/// ドメイン例外の基底クラス
/// </summary>
public abstract class DomainException : Exception
{
    protected DomainException(string message) : base(message) { }

    protected DomainException(string message, Exception innerException)
        : base(message, innerException) { }
}
