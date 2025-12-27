using ProductionManagement.Domain.Models.Supplier;

namespace ProductionManagement.Application.Port.In;

/// <summary>
/// 取引先ユースケース（Input Port）
/// </summary>
public interface ISupplierUseCase
{
    /// <summary>
    /// 取引先を登録する
    /// </summary>
    Task<Supplier> CreateSupplierAsync(CreateSupplierCommand command);

    /// <summary>
    /// すべての取引先を取得する
    /// </summary>
    Task<IReadOnlyList<Supplier>> GetAllSuppliersAsync();

    /// <summary>
    /// 取引先コードで取引先を取得する
    /// </summary>
    Task<Supplier> GetSupplierByCodeAsync(string supplierCode);

    /// <summary>
    /// 取引先区分で取引先を取得する
    /// </summary>
    Task<IReadOnlyList<Supplier>> GetSuppliersByTypeAsync(SupplierType supplierType);
}

/// <summary>
/// 取引先登録コマンド
/// </summary>
public record CreateSupplierCommand(
    string SupplierCode,
    string SupplierName,
    SupplierType SupplierType,
    string? SupplierNameKana = null,
    string? PostalCode = null,
    string? Address = null,
    string? PhoneNumber = null,
    string? FaxNumber = null,
    string? ContactPerson = null
);
