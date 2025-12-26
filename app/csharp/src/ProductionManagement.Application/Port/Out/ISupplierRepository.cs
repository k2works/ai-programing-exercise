using ProductionManagement.Domain.Models.Supplier;

namespace ProductionManagement.Application.Port.Out;

/// <summary>
/// 取引先リポジトリ（Output Port）
/// </summary>
public interface ISupplierRepository
{
    /// <summary>
    /// 取引先を保存する
    /// </summary>
    Task SaveAsync(Supplier supplier);

    /// <summary>
    /// 取引先コードで取引先を検索する（最新の適用開始日）
    /// </summary>
    Task<Supplier?> FindByCodeAsync(string supplierCode);

    /// <summary>
    /// 取引先コードと基準日で取引先を検索する
    /// </summary>
    Task<Supplier?> FindByCodeAndDateAsync(string supplierCode, DateOnly baseDate);

    /// <summary>
    /// 取引先区分で取引先を検索する
    /// </summary>
    Task<IReadOnlyList<Supplier>> FindByTypeAsync(SupplierType supplierType);

    /// <summary>
    /// すべての取引先を取得する
    /// </summary>
    Task<IReadOnlyList<Supplier>> FindAllAsync();

    /// <summary>
    /// すべての取引先を削除する
    /// </summary>
    Task DeleteAllAsync();
}
