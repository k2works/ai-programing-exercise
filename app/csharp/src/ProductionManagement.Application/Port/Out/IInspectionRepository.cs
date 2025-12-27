using ProductionManagement.Domain.Models.Purchase;

namespace ProductionManagement.Application.Port.Out;

/// <summary>
/// 受入検査リポジトリインターフェース
/// </summary>
public interface IInspectionRepository
{
    Task SaveAsync(Inspection inspection);
    Task<Inspection?> FindByIdAsync(int id);
    Task<Inspection?> FindByInspectionNumberAsync(string inspectionNumber);
    Task<IReadOnlyList<Inspection>> FindByReceivingNumberAsync(string receivingNumber);
    Task<string?> FindLatestInspectionNumberAsync(string prefix);
    Task DeleteAllAsync();
}
