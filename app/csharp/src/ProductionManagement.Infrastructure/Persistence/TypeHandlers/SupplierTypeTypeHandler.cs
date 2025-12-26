using System.Data;
using Dapper;
using ProductionManagement.Domain.Models.Supplier;

namespace ProductionManagement.Infrastructure.Persistence.TypeHandlers;

/// <summary>
/// SupplierType と日本語 ENUM 値の変換を行う TypeHandler
/// </summary>
public class SupplierTypeTypeHandler : SqlMapper.TypeHandler<SupplierType>
{
    public override void SetValue(IDbDataParameter parameter, SupplierType value)
    {
        parameter.Value = value.GetDisplayName();
    }

    public override SupplierType Parse(object value)
    {
        return SupplierTypeExtensions.FromDisplayName(value.ToString()!);
    }
}
