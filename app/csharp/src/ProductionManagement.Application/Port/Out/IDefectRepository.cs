using ProductionManagement.Domain.Models.Purchase;

namespace ProductionManagement.Application.Port.Out;

/// <summary>
/// 欠点マスタリポジトリインターフェース
/// </summary>
public interface IDefectRepository
{
    Task SaveAsync(Defect defect);
    Task<Defect?> FindByIdAsync(int id);
    Task<Defect?> FindByDefectCodeAsync(string defectCode);
    Task<IReadOnlyList<Defect>> FindAllActiveAsync();
    Task DeleteAllAsync();
}
