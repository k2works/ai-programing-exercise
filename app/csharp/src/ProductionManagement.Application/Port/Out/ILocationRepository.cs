using ProductionManagement.Domain.Models.Location;

namespace ProductionManagement.Application.Port.Out;

/// <summary>
/// 場所リポジトリ（Output Port）
/// </summary>
public interface ILocationRepository
{
    /// <summary>
    /// 場所を保存する
    /// </summary>
    Task SaveAsync(Location location);

    /// <summary>
    /// 場所コードで場所を検索する
    /// </summary>
    Task<Location?> FindByCodeAsync(string locationCode);

    /// <summary>
    /// 場所区分で場所を検索する
    /// </summary>
    Task<IReadOnlyList<Location>> FindByTypeAsync(LocationType locationType);

    /// <summary>
    /// 親場所コードで子場所を検索する
    /// </summary>
    Task<IReadOnlyList<Location>> FindChildrenAsync(string parentLocationCode);

    /// <summary>
    /// すべての場所を取得する
    /// </summary>
    Task<IReadOnlyList<Location>> FindAllAsync();

    /// <summary>
    /// すべての場所を削除する
    /// </summary>
    Task DeleteAllAsync();
}
