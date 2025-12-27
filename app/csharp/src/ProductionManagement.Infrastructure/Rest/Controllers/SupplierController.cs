using Microsoft.AspNetCore.Http;
using Microsoft.AspNetCore.Mvc;
using ProductionManagement.Application.Port.In;
using ProductionManagement.Domain.Models.Supplier;
using ProductionManagement.Infrastructure.Rest.Dto;

namespace ProductionManagement.Infrastructure.Rest.Controllers;

/// <summary>
/// 取引先 Controller（Input Adapter）
/// </summary>
[ApiController]
[Route("api/suppliers")]
[Tags("Suppliers")]
public class SupplierController : ControllerBase
{
    private readonly ISupplierUseCase _supplierUseCase;

    public SupplierController(ISupplierUseCase supplierUseCase)
    {
        _supplierUseCase = supplierUseCase;
    }

    /// <summary>
    /// 取引先一覧の取得
    /// </summary>
    [HttpGet]
    [ProducesResponseType(typeof(IReadOnlyList<SupplierResponse>), StatusCodes.Status200OK)]
    public async Task<IActionResult> GetAllSuppliers([FromQuery] string? type = null)
    {
        if (!string.IsNullOrEmpty(type))
        {
            var supplierType = SupplierTypeExtensions.FromDisplayName(type);
            var suppliers = await _supplierUseCase.GetSuppliersByTypeAsync(supplierType);
            return Ok(suppliers.Select(SupplierResponse.From).ToList());
        }

        var allSuppliers = await _supplierUseCase.GetAllSuppliersAsync();
        return Ok(allSuppliers.Select(SupplierResponse.From).ToList());
    }

    /// <summary>
    /// 取引先の取得
    /// </summary>
    /// <param name="supplierCode">取引先コード</param>
    [HttpGet("{supplierCode}")]
    [ProducesResponseType(typeof(SupplierResponse), StatusCodes.Status200OK)]
    [ProducesResponseType(StatusCodes.Status404NotFound)]
    public async Task<IActionResult> GetSupplier(string supplierCode)
    {
        var supplier = await _supplierUseCase.GetSupplierByCodeAsync(supplierCode);
        return Ok(SupplierResponse.From(supplier));
    }

    /// <summary>
    /// 取引先の登録
    /// </summary>
    [HttpPost]
    [ProducesResponseType(typeof(SupplierResponse), StatusCodes.Status201Created)]
    [ProducesResponseType(StatusCodes.Status400BadRequest)]
    [ProducesResponseType(StatusCodes.Status409Conflict)]
    public async Task<IActionResult> CreateSupplier([FromBody] CreateSupplierRequest request)
    {
        var supplierType = SupplierTypeExtensions.FromDisplayName(request.SupplierType);
        var command = new CreateSupplierCommand(
            SupplierCode: request.SupplierCode,
            SupplierName: request.SupplierName,
            SupplierType: supplierType,
            SupplierNameKana: request.SupplierNameKana,
            PostalCode: request.PostalCode,
            Address: request.Address,
            PhoneNumber: request.PhoneNumber,
            FaxNumber: request.FaxNumber,
            ContactPerson: request.ContactPerson
        );

        var supplier = await _supplierUseCase.CreateSupplierAsync(command);
        return CreatedAtAction(
            nameof(GetSupplier),
            new { supplierCode = supplier.SupplierCode },
            SupplierResponse.From(supplier)
        );
    }
}
