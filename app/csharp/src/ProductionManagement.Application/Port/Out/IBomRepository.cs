using ProductionManagement.Domain.Models.Bom;

namespace ProductionManagement.Application.Port.Out;

/// <summary>
/// BOMリポジトリ（Output Port）
/// </summary>
public interface IBomRepository
{
    /// <summary>
    /// BOM を保存する
    /// </summary>
    Task SaveAsync(Bom bom);

    /// <summary>
    /// 親品目コードで BOM を検索する
    /// </summary>
    Task<IReadOnlyList<Bom>> FindByParentItemCodeAsync(string parentItemCode);

    /// <summary>
    /// 親品目コードと基準日で BOM を検索する
    /// </summary>
    Task<IReadOnlyList<Bom>> FindByParentItemCodeAndDateAsync(string parentItemCode, DateOnly baseDate);

    /// <summary>
    /// 子品目コードで BOM を検索する（逆展開）
    /// </summary>
    Task<IReadOnlyList<Bom>> FindByChildItemCodeAsync(string childItemCode);

    /// <summary>
    /// BOM を再帰的に展開する
    /// </summary>
    Task<IReadOnlyList<BomExplosion>> ExplodeAsync(string itemCode, decimal quantity);

    /// <summary>
    /// すべての BOM を削除する
    /// </summary>
    Task DeleteAllAsync();
}
