using ProductionManagement.Domain.Models.Quality;

namespace ProductionManagement.Application.Port.Out;

/// <summary>
/// 出荷検査リポジトリインターフェース
/// </summary>
public interface IShipmentInspectionRepository
{
    Task<ShipmentInspection?> FindByInspectionNumberAsync(string inspectionNumber);

    Task<IReadOnlyList<ShipmentInspection>> FindByShipmentNumberAsync(string shipmentNumber);

    Task<IReadOnlyList<ShipmentInspectionResult>> FindResultsByInspectionNumberAsync(string inspectionNumber);

    Task<string?> FindLatestInspectionNumberAsync(string pattern);

    Task<long> SaveAsync(ShipmentInspection inspection);

    Task SaveResultAsync(ShipmentInspectionResult result);

    Task DeleteAllResultsAsync();

    Task DeleteAllAsync();
}
