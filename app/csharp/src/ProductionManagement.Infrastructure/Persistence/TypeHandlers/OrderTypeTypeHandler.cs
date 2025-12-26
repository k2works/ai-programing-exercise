using System.Data;
using Dapper;
using ProductionManagement.Domain.Models.Plan;

namespace ProductionManagement.Infrastructure.Persistence.TypeHandlers;

/// <summary>
/// OrderType と日本語 ENUM 値の変換を行う TypeHandler
/// </summary>
public class OrderTypeTypeHandler : SqlMapper.TypeHandler<OrderType>
{
    public override void SetValue(IDbDataParameter parameter, OrderType value)
    {
        parameter.Value = value.GetDisplayName();
    }

    public override OrderType Parse(object value)
    {
        return OrderTypeExtensions.FromDisplayName(value.ToString()!);
    }
}
