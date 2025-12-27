using Microsoft.AspNetCore.Mvc;
using Microsoft.AspNetCore.Mvc.Filters;
using Microsoft.AspNetCore.Mvc.ViewFeatures;
using ProductionManagement.Domain.Exceptions;

namespace ProductionManagement.Web.Filters;

/// <summary>
/// ドメイン例外をハンドリングするフィルター
/// </summary>
public class DomainExceptionFilter : IExceptionFilter
{
    private readonly ILogger<DomainExceptionFilter> _logger;
    private readonly ITempDataDictionaryFactory _tempDataDictionaryFactory;

    public DomainExceptionFilter(
        ILogger<DomainExceptionFilter> logger,
        ITempDataDictionaryFactory tempDataDictionaryFactory)
    {
        _logger = logger;
        _tempDataDictionaryFactory = tempDataDictionaryFactory;
    }

    public void OnException(ExceptionContext context)
    {
        switch (context.Exception)
        {
            // NotFound 系の例外
            case ItemNotFoundException ex:
                HandleNotFound(context, "品目", ex.Message);
                break;

            case PurchaseOrderNotFoundException ex:
                HandleNotFound(context, "発注", ex.Message);
                break;

            case WorkOrderNotFoundException ex:
                HandleNotFound(context, "作業指示", ex.Message);
                break;

            case SupplierNotFoundException ex:
                HandleNotFound(context, "取引先", ex.Message);
                break;

            case OrderNotFoundException ex:
                HandleNotFound(context, "オーダ", ex.Message);
                break;

            case ResourceNotFoundException ex:
                HandleNotFound(context, "リソース", ex.Message);
                break;

            // 重複エラー系の例外
            case DuplicateItemException ex:
                HandleDuplicateError(context, "品目コード", ex.Message);
                break;

            case DuplicateSupplierException ex:
                HandleDuplicateError(context, "取引先コード", ex.Message);
                break;

            // 在庫不足系の例外
            case InsufficientStockException ex:
                HandleBusinessError(context, ex.Message);
                break;

            case InsufficientInventoryException ex:
                HandleBusinessError(context, ex.Message);
                break;

            // その他のドメイン例外
            case DomainException ex:
                HandleBusinessError(context, ex.Message);
                break;
        }
    }

    private void HandleNotFound(ExceptionContext context, string resourceType, string message)
    {
        _logger.LogWarning("{ResourceType} not found: {Message}", resourceType, message);

        var tempData = _tempDataDictionaryFactory.GetTempData(context.HttpContext);
        tempData["Error"] = $"{resourceType}が見つかりません: {message}";

        context.Result = new NotFoundResult();
        context.ExceptionHandled = true;
    }

    private void HandleDuplicateError(ExceptionContext context, string fieldName, string message)
    {
        _logger.LogWarning("Duplicate error on {FieldName}: {Message}", fieldName, message);

        var tempData = _tempDataDictionaryFactory.GetTempData(context.HttpContext);
        tempData["Error"] = message;

        // フォームに戻すためのリダイレクトは Controller で処理するため
        // ここでは ExceptionHandled を true にしない（Controller の catch で処理）
    }

    private void HandleBusinessError(ExceptionContext context, string message)
    {
        _logger.LogWarning("Business error: {Message}", message);

        var tempData = _tempDataDictionaryFactory.GetTempData(context.HttpContext);
        tempData["Error"] = message;

        // 前の画面にリダイレクト
        var referer = context.HttpContext.Request.Headers.Referer.ToString();
        if (!string.IsNullOrEmpty(referer))
        {
            context.Result = new RedirectResult(referer);
        }
        else
        {
            context.Result = new RedirectToActionResult("Index", "Home", null);
        }

        context.ExceptionHandled = true;
    }
}
