using System.Data;
using Dapper;
using ProductionManagement.Domain.Models.Purchase;

namespace ProductionManagement.Infrastructure.Persistence.TypeHandlers;

/// <summary>
/// InspectionResult と日本語 ENUM 値の変換を行う TypeHandler
/// </summary>
public class InspectionResultTypeHandler : SqlMapper.TypeHandler<InspectionResult>
{
    public override void SetValue(IDbDataParameter parameter, InspectionResult value)
    {
        parameter.Value = value.GetDisplayName();
    }

    public override InspectionResult Parse(object value)
    {
        return InspectionResultExtensions.FromDisplayName(value.ToString()!);
    }
}
