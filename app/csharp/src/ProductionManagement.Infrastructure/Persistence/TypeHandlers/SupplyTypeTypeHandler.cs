using System.Data;
using Dapper;
using ProductionManagement.Domain.Models.Subcontract;

namespace ProductionManagement.Infrastructure.Persistence.TypeHandlers;

/// <summary>
/// SupplyType と日本語 ENUM 値の変換を行う TypeHandler
/// </summary>
public class SupplyTypeTypeHandler : SqlMapper.TypeHandler<SupplyType>
{
    public override void SetValue(IDbDataParameter parameter, SupplyType value)
    {
        parameter.Value = value.GetDisplayName();
    }

    public override SupplyType Parse(object value)
    {
        return SupplyTypeExtensions.FromDisplayName(value.ToString()!);
    }
}
