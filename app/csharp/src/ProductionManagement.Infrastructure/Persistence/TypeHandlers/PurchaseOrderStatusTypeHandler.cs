using System.Data;
using Dapper;
using ProductionManagement.Domain.Models.Purchase;

namespace ProductionManagement.Infrastructure.Persistence.TypeHandlers;

/// <summary>
/// PurchaseOrderStatus と日本語 ENUM 値の変換を行う TypeHandler
/// </summary>
public class PurchaseOrderStatusTypeHandler : SqlMapper.TypeHandler<PurchaseOrderStatus>
{
    public override void SetValue(IDbDataParameter parameter, PurchaseOrderStatus value)
    {
        parameter.Value = value.GetDisplayName();
    }

    public override PurchaseOrderStatus Parse(object value)
    {
        return PurchaseOrderStatusExtensions.FromDisplayName(value.ToString()!);
    }
}
