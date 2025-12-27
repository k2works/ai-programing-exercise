using System.Data;
using Dapper;
using ProductionManagement.Domain.Models.Inventory;

namespace ProductionManagement.Infrastructure.Persistence.TypeHandlers;

/// <summary>
/// 棚卸ステータスの TypeHandler
/// </summary>
public class StocktakingStatusTypeHandler : SqlMapper.TypeHandler<StocktakingStatus>
{
    public override StocktakingStatus Parse(object value)
    {
        return StocktakingStatusExtensions.FromDisplayName(value.ToString()!);
    }

    public override void SetValue(IDbDataParameter parameter, StocktakingStatus value)
    {
        parameter.Value = value.ToDisplayName();
    }
}
