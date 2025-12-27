using System.Data;
using Dapper;
using ProductionManagement.Domain.Models.Process;

namespace ProductionManagement.Infrastructure.Persistence.TypeHandlers;

/// <summary>
/// 作業指示ステータスの TypeHandler
/// </summary>
public class WorkOrderStatusTypeHandler : SqlMapper.TypeHandler<WorkOrderStatus>
{
    public override WorkOrderStatus Parse(object value)
    {
        return WorkOrderStatusExtensions.FromDisplayName(value.ToString()!);
    }

    public override void SetValue(IDbDataParameter parameter, WorkOrderStatus value)
    {
        parameter.Value = value.GetDisplayName();
    }
}
