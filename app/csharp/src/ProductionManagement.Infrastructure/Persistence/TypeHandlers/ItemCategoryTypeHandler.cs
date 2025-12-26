using System.Data;
using Dapper;
using ProductionManagement.Domain.Models.Item;

namespace ProductionManagement.Infrastructure.Persistence.TypeHandlers;

/// <summary>
/// ItemCategory と日本語 ENUM 値の変換を行う TypeHandler
/// </summary>
public class ItemCategoryTypeHandler : SqlMapper.TypeHandler<ItemCategory>
{
    public override void SetValue(IDbDataParameter parameter, ItemCategory value)
    {
        parameter.Value = value.GetDisplayName();
    }

    public override ItemCategory Parse(object value)
    {
        return ItemCategoryExtensions.FromDisplayName(value.ToString()!);
    }
}
