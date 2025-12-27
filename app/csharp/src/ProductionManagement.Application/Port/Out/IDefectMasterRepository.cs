using ProductionManagement.Domain.Models.Quality;

namespace ProductionManagement.Application.Port.Out;

/// <summary>
/// 欠点マスタリポジトリインターフェース
/// </summary>
public interface IDefectMasterRepository
{
    Task<Defect?> FindByDefectCodeAsync(string defectCode);

    Task<IReadOnlyList<Defect>> FindAllAsync();

    Task SaveAsync(Defect defect);

    Task DeleteAllAsync();
}
