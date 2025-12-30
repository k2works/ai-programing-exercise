using ProductionManagement.Domain.Models.Item;

namespace ProductionManagement.Application.Port.In;

/// <summary>
/// 品目ユースケース（Input Port）
/// </summary>
public interface IItemUseCase
{
    /// <summary>
    /// 品目を登録する
    /// </summary>
    Task<Item> CreateItemAsync(CreateItemCommand command);

    /// <summary>
    /// 品目を更新する
    /// </summary>
    Task<Item> UpdateItemAsync(UpdateItemCommand command);

    /// <summary>
    /// すべての品目を取得する
    /// </summary>
    Task<IReadOnlyList<Item>> GetAllItemsAsync();

    /// <summary>
    /// 品目コードで品目を取得する
    /// </summary>
    Task<Item> GetItemByCodeAsync(string itemCode);

    /// <summary>
    /// 品目区分で品目を取得する
    /// </summary>
    Task<IReadOnlyList<Item>> GetItemsByCategoryAsync(ItemCategory category);

    /// <summary>
    /// 品目を削除する
    /// </summary>
    Task DeleteItemAsync(string itemCode);

    /// <summary>
    /// キーワードで品目を検索する
    /// </summary>
    Task<IReadOnlyList<Item>> SearchItemsAsync(string keyword);
}
