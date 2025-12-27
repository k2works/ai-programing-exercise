using ProductionManagement.Application.Port.In;
using ProductionManagement.Application.Port.Out;
using ProductionManagement.Domain.Exceptions;
using ProductionManagement.Domain.Models.Supplier;

namespace ProductionManagement.Application.Services;

/// <summary>
/// 取引先アプリケーションサービス
/// </summary>
public class SupplierService : ISupplierUseCase
{
    private readonly ISupplierRepository _supplierRepository;

    public SupplierService(ISupplierRepository supplierRepository)
    {
        _supplierRepository = supplierRepository;
    }

    public async Task<Supplier> CreateSupplierAsync(CreateSupplierCommand command)
    {
        // 重複チェック
        var existing = await _supplierRepository.FindByCodeAsync(command.SupplierCode);
        if (existing is not null)
        {
            throw new DuplicateSupplierException(command.SupplierCode);
        }

        var supplier = new Supplier
        {
            SupplierCode = command.SupplierCode,
            EffectiveFrom = DateOnly.FromDateTime(DateTime.Today),
            SupplierName = command.SupplierName,
            SupplierType = command.SupplierType,
            SupplierNameKana = command.SupplierNameKana,
            PostalCode = command.PostalCode,
            Address = command.Address,
            PhoneNumber = command.PhoneNumber,
            FaxNumber = command.FaxNumber,
            ContactPerson = command.ContactPerson
        };

        await _supplierRepository.SaveAsync(supplier);
        return supplier;
    }

    public async Task<IReadOnlyList<Supplier>> GetAllSuppliersAsync()
    {
        return await _supplierRepository.FindAllAsync();
    }

    public async Task<Supplier> GetSupplierByCodeAsync(string supplierCode)
    {
        return await _supplierRepository.FindByCodeAsync(supplierCode)
            ?? throw new SupplierNotFoundException(supplierCode);
    }

    public async Task<IReadOnlyList<Supplier>> GetSuppliersByTypeAsync(SupplierType supplierType)
    {
        return await _supplierRepository.FindByTypeAsync(supplierType);
    }
}
