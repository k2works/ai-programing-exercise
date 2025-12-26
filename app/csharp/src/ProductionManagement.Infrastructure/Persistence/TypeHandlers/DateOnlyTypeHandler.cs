using System.Data;
using Dapper;

namespace ProductionManagement.Infrastructure.Persistence.TypeHandlers;

/// <summary>
/// DateOnly と DateTime の変換を行う TypeHandler
/// Npgsql 10.0+ では DateOnly がネイティブサポートされているため、型チェックが必要
/// </summary>
public class DateOnlyTypeHandler : SqlMapper.TypeHandler<DateOnly>
{
    public override void SetValue(IDbDataParameter parameter, DateOnly value)
    {
        parameter.Value = value;
    }

    public override DateOnly Parse(object value)
    {
        return value switch
        {
            DateOnly dateOnly => dateOnly,
            DateTime dateTime => DateOnly.FromDateTime(dateTime),
            _ => throw new InvalidCastException($"Cannot convert {value.GetType()} to DateOnly")
        };
    }
}
