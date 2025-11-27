using Refit;

namespace ManagementAccounting.Infrastructure.Adapters.External;

/// <summary>
/// 財務会計サービス API インターフェース（Refit による型安全な HTTP クライアント）
/// </summary>
public interface IFinancialAccountingApi
{
    [Get("/api/journals")]
    Task<List<JournalDto>> GetJournalsByFiscalYearAsync([Query] int fiscalYear);

    [Get("/api/journals/{id}")]
    Task<JournalDto> GetJournalByIdAsync(int id);
}
