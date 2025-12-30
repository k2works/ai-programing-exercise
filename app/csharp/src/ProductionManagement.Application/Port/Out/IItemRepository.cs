using ProductionManagement.Domain.Models.Item;

namespace ProductionManagement.Application.Port.Out;

/// <summary>
/// 品目リポジトリ（Output Port）
/// ドメイン層がデータアクセスに依存しないためのインターフェース
/// </summary>
public interface IItemRepository
{
    /// <summary>
    /// 品目を保存する
    /// </summary>
    Task SaveAsync(Item item);

    /// <summary>
    /// 品目コードで品目を検索する（最新の適用開始日）
    /// </summary>
    Task<Item?> FindByItemCodeAsync(string itemCode);

    /// <summary>
    /// 品目コードと基準日で品目を検索する
    /// </summary>
    Task<Item?> FindByItemCodeAndDateAsync(string itemCode, DateOnly baseDate);

    /// <summary>
    /// すべての品目を取得する
    /// </summary>
    Task<IReadOnlyList<Item>> FindAllAsync();

    /// <summary>
    /// 品目を更新する
    /// </summary>
    Task UpdateAsync(Item item);

    /// <summary>
    /// 品目区分で品目を取得する
    /// </summary>
    Task<IReadOnlyList<Item>> FindByCategoryAsync(ItemCategory category);

    /// <summary>
    /// 品目コードで品目を削除する
    /// </summary>
    Task DeleteByCodeAsync(string itemCode);

    /// <summary>
    /// すべての品目を削除する
    /// </summary>
    Task DeleteAllAsync();

    /// <summary>
    /// キーワードで品目を検索する（品目コードまたは品名に部分一致）
    /// </summary>
    Task<IReadOnlyList<Item>> SearchAsync(string keyword);
}
