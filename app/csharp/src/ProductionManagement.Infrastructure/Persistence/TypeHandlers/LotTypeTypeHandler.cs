using System.Data;
using Dapper;
using ProductionManagement.Domain.Models.Quality;

namespace ProductionManagement.Infrastructure.Persistence.TypeHandlers;

/// <summary>
/// ロット種別の TypeHandler
/// </summary>
public class LotTypeTypeHandler : SqlMapper.TypeHandler<LotType>
{
    public override LotType Parse(object value)
    {
        return LotTypeExtensions.FromDisplayName(value.ToString()!);
    }

    public override void SetValue(IDbDataParameter parameter, LotType value)
    {
        parameter.Value = value.ToDisplayName();
    }
}
