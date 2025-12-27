namespace ProductionManagement.Domain.Exceptions;

/// <summary>
/// リソースが見つからない例外
/// </summary>
public class ResourceNotFoundException : Exception
{
    public ResourceNotFoundException(string message) : base(message) { }
}
