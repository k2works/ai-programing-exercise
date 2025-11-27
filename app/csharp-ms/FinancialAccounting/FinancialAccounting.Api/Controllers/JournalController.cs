using FinancialAccounting.Application.Ports.In;
using Microsoft.AspNetCore.Mvc;

namespace FinancialAccounting.Api.Controllers;

/// <summary>
/// 仕訳 API コントローラー
/// </summary>
[ApiController]
[Route("api/journals")]
public class JournalController : ControllerBase
{
    private readonly ICreateJournalUseCase _createJournalUseCase;
    private readonly IGetJournalUseCase _getJournalUseCase;

    public JournalController(
        ICreateJournalUseCase createJournalUseCase,
        IGetJournalUseCase getJournalUseCase)
    {
        _createJournalUseCase = createJournalUseCase;
        _getJournalUseCase = getJournalUseCase;
    }

    /// <summary>
    /// 仕訳を作成
    /// </summary>
    [HttpPost]
    public async Task<ActionResult<JournalResponse>> CreateJournal([FromBody] CreateJournalRequest request)
    {
        var journal = await _createJournalUseCase.CreateJournalAsync(
            request.JournalDate,
            request.Description,
            request.FiscalYear,
            request.Entries);

        return CreatedAtAction(
            nameof(GetJournal),
            new { id = journal.JournalId },
            JournalResponse.FromEntity(journal));
    }

    /// <summary>
    /// 仕訳を取得
    /// </summary>
    [HttpGet("{id}")]
    public async Task<ActionResult<JournalResponse>> GetJournal(int id)
    {
        var journal = await _getJournalUseCase.GetJournalByIdAsync(id);

        if (journal == null)
            return NotFound();

        return Ok(JournalResponse.FromEntity(journal));
    }

    /// <summary>
    /// 会計年度の仕訳一覧を取得
    /// </summary>
    [HttpGet]
    public async Task<ActionResult<List<JournalResponse>>> GetJournalsByFiscalYear([FromQuery] int fiscalYear)
    {
        var journals = await _getJournalUseCase.GetJournalsByFiscalYearAsync(fiscalYear);

        return Ok(journals.Select(JournalResponse.FromEntity).ToList());
    }
}

/// <summary>
/// 仕訳作成リクエスト
/// </summary>
public record CreateJournalRequest(
    DateTime JournalDate,
    string Description,
    int FiscalYear,
    List<JournalEntryRequest> Entries
);

/// <summary>
/// 仕訳レスポンス
/// </summary>
public record JournalResponse(
    int JournalId,
    DateTime JournalDate,
    string Description,
    int FiscalYear,
    List<JournalEntryResponse> Entries
)
{
    public static JournalResponse FromEntity(FinancialAccounting.Domain.Entities.Journal journal) =>
        new(
            journal.JournalId!.Value,
            journal.JournalDate,
            journal.Description,
            journal.FiscalYear,
            journal.Entries.Select(e => new JournalEntryResponse(
                e.EntryId ?? 0,
                e.AccountCode,
                e.DebitAmount,
                e.CreditAmount,
                e.Description
            )).ToList()
        );
}

/// <summary>
/// 仕訳明細レスポンス
/// </summary>
public record JournalEntryResponse(
    int EntryId,
    string AccountCode,
    decimal DebitAmount,
    decimal CreditAmount,
    string? Description
);
