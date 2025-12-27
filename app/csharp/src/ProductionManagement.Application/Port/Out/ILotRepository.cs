using ProductionManagement.Domain.Models.Quality;

namespace ProductionManagement.Application.Port.Out;

/// <summary>
/// ロットリポジトリインターフェース
/// </summary>
public interface ILotRepository
{
    Task<LotMaster?> FindByLotNumberAsync(string lotNumber);

    Task<IReadOnlyList<LotMaster>> FindByItemCodeAsync(string itemCode);

    Task<IReadOnlyList<LotComposition>> FindChildLotsAsync(string parentLotNumber);

    Task<IReadOnlyList<LotComposition>> FindParentLotsAsync(string childLotNumber);

    Task<long> SaveAsync(LotMaster lot);

    Task SaveCompositionAsync(LotComposition composition);

    Task DeleteAllCompositionsAsync();

    Task DeleteAllAsync();
}
