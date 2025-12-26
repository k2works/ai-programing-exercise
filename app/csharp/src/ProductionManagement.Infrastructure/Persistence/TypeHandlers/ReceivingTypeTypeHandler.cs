using System.Data;
using Dapper;
using ProductionManagement.Domain.Models.Purchase;

namespace ProductionManagement.Infrastructure.Persistence.TypeHandlers;

/// <summary>
/// ReceivingType と日本語 ENUM 値の変換を行う TypeHandler
/// </summary>
public class ReceivingTypeTypeHandler : SqlMapper.TypeHandler<ReceivingType>
{
    public override void SetValue(IDbDataParameter parameter, ReceivingType value)
    {
        parameter.Value = value.GetDisplayName();
    }

    public override ReceivingType Parse(object value)
    {
        return ReceivingTypeExtensions.FromDisplayName(value.ToString()!);
    }
}
