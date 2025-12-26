using System.Data;
using Dapper;
using ProductionManagement.Domain.Models.Plan;

namespace ProductionManagement.Infrastructure.Persistence.TypeHandlers;

/// <summary>
/// PlanStatus と日本語 ENUM 値の変換を行う TypeHandler
/// </summary>
public class PlanStatusTypeHandler : SqlMapper.TypeHandler<PlanStatus>
{
    public override void SetValue(IDbDataParameter parameter, PlanStatus value)
    {
        parameter.Value = value.GetDisplayName();
    }

    public override PlanStatus Parse(object value)
    {
        return PlanStatusExtensions.FromDisplayName(value.ToString()!);
    }
}
