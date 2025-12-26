using System.Data;
using Dapper;
using ProductionManagement.Domain.Models.Plan;

namespace ProductionManagement.Infrastructure.Persistence.TypeHandlers;

/// <summary>
/// AllocationType と日本語 ENUM 値の変換を行う TypeHandler
/// </summary>
public class AllocationTypeTypeHandler : SqlMapper.TypeHandler<AllocationType>
{
    public override void SetValue(IDbDataParameter parameter, AllocationType value)
    {
        parameter.Value = value.GetDisplayName();
    }

    public override AllocationType Parse(object value)
    {
        return AllocationTypeExtensions.FromDisplayName(value.ToString()!);
    }
}
