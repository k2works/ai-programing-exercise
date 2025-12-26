using ProductionManagement.Domain.Models.Purchase;

namespace ProductionManagement.Application.Port.Out;

/// <summary>
/// 単価マスタリポジトリインターフェース
/// </summary>
public interface IUnitPriceRepository
{
    Task SaveAsync(UnitPrice unitPrice);
    Task<UnitPrice?> FindEffectiveUnitPriceAsync(string itemCode, string supplierCode, DateOnly date);
    Task DeleteAllAsync();
}
