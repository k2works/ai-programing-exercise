namespace ProductionManagement.Domain.Exceptions;

/// <summary>
/// 作業指示が見つからない例外
/// </summary>
public class WorkOrderNotFoundException : DomainException
{
    public string WorkOrderNumber { get; }

    public WorkOrderNotFoundException(string workOrderNumber)
        : base($"作業指示が見つかりません: {workOrderNumber}")
    {
        WorkOrderNumber = workOrderNumber;
    }
}
