using System.Data;
using Dapper;
using ProductionManagement.Domain.Models.Location;

namespace ProductionManagement.Infrastructure.Persistence.TypeHandlers;

/// <summary>
/// LocationType と日本語 ENUM 値の変換を行う TypeHandler
/// </summary>
public class LocationTypeTypeHandler : SqlMapper.TypeHandler<LocationType>
{
    public override void SetValue(IDbDataParameter parameter, LocationType value)
    {
        parameter.Value = value.GetDisplayName();
    }

    public override LocationType Parse(object value)
    {
        return LocationTypeExtensions.FromDisplayName(value.ToString()!);
    }
}
