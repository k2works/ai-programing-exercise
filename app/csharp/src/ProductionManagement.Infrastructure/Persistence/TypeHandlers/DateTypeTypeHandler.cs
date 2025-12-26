using System.Data;
using Dapper;
using ProductionManagement.Domain.Models.Calendar;

namespace ProductionManagement.Infrastructure.Persistence.TypeHandlers;

/// <summary>
/// DateType と日本語 ENUM 値の変換を行う TypeHandler
/// </summary>
public class DateTypeTypeHandler : SqlMapper.TypeHandler<DateType>
{
    public override void SetValue(IDbDataParameter parameter, DateType value)
    {
        parameter.Value = value.GetDisplayName();
    }

    public override DateType Parse(object value)
    {
        return DateTypeExtensions.FromDisplayName(value.ToString()!);
    }
}
