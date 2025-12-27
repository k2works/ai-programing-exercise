using System.Data;
using Dapper;
using ProductionManagement.Domain.Models.Quality;

namespace ProductionManagement.Infrastructure.Persistence.TypeHandlers;

/// <summary>
/// 検査判定の TypeHandler
/// </summary>
public class InspectionJudgmentTypeHandler : SqlMapper.TypeHandler<InspectionJudgment>
{
    public override InspectionJudgment Parse(object value)
    {
        return InspectionJudgmentExtensions.FromDisplayName(value.ToString()!);
    }

    public override void SetValue(IDbDataParameter parameter, InspectionJudgment value)
    {
        parameter.Value = value.ToDisplayName();
    }
}
