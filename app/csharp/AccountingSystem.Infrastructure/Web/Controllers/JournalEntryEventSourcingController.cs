namespace AccountingSystem.Infrastructure.Web.Controllers;

using AccountingSystem.Application.Ports.In;
using Microsoft.AspNetCore.Http;
using Microsoft.AspNetCore.Mvc;

/// <summary>
/// イベントソーシング版仕訳 API コントローラー
/// </summary>
[ApiController]
[Route("api/v1/journal-entries-es")]
[Tags("Journal Entry Event Sourcing")]
public class JournalEntryEventSourcingController : ControllerBase
{
    private readonly IJournalEntryEventSourcingService _journalEntryService;

    public JournalEntryEventSourcingController(IJournalEntryEventSourcingService journalEntryService)
    {
        _journalEntryService = journalEntryService;
    }

    /// <summary>
    /// 仕訳を作成する（イベントソーシング）
    /// </summary>
    [HttpPost]
    [ProducesResponseType(typeof(CreateJournalEntryResponse), StatusCodes.Status201Created)]
    [ProducesResponseType(StatusCodes.Status400BadRequest)]
    public async Task<IActionResult> CreateJournalEntry(
        [FromBody] CreateJournalEntryRequest request)
    {
        var id = await _journalEntryService.CreateJournalEntryAsync(
            request.EntryDate,
            request.Description,
            request.LineItems.Select(item => new LineItemDto
            {
                AccountCode = item.AccountCode,
                DebitCredit = item.DebitCredit,
                Amount = item.Amount
            }).ToList(),
            request.UserId
        );

        return CreatedAtAction(
            nameof(GetJournalEntry),
            new { id },
            new CreateJournalEntryResponse(id)
        );
    }

    /// <summary>
    /// 仕訳を承認する
    /// </summary>
    [HttpPost("{id}/approve")]
    [ProducesResponseType(StatusCodes.Status204NoContent)]
    [ProducesResponseType(StatusCodes.Status400BadRequest)]
    [ProducesResponseType(StatusCodes.Status404NotFound)]
    public async Task<IActionResult> ApproveJournalEntry(
        string id,
        [FromBody] ApproveJournalEntryRequest request)
    {
        try
        {
            await _journalEntryService.ApproveJournalEntryAsync(id, request.ApprovedBy, request.Comment);
            return NoContent();
        }
        catch (ArgumentException ex) when (ex.Message.Contains("見つかりません"))
        {
            return NotFound(new { message = ex.Message });
        }
        catch (InvalidOperationException ex)
        {
            return BadRequest(new { message = ex.Message });
        }
    }

    /// <summary>
    /// 仕訳を削除する
    /// </summary>
    [HttpDelete("{id}")]
    [ProducesResponseType(StatusCodes.Status204NoContent)]
    [ProducesResponseType(StatusCodes.Status404NotFound)]
    public async Task<IActionResult> DeleteJournalEntry(
        string id,
        [FromBody] DeleteJournalEntryRequest request)
    {
        try
        {
            await _journalEntryService.DeleteJournalEntryAsync(id, request.Reason, request.UserId);
            return NoContent();
        }
        catch (ArgumentException ex) when (ex.Message.Contains("見つかりません"))
        {
            return NotFound(new { message = ex.Message });
        }
    }

    /// <summary>
    /// 仕訳を取得する（イベント再生）
    /// </summary>
    [HttpGet("{id}")]
    [ProducesResponseType(typeof(JournalEntryResponse), StatusCodes.Status200OK)]
    [ProducesResponseType(StatusCodes.Status404NotFound)]
    public async Task<IActionResult> GetJournalEntry(string id)
    {
        try
        {
            var aggregate = await _journalEntryService.GetJournalEntryAsync(id);

            var response = new JournalEntryResponse
            {
                Id = aggregate.Id,
                EntryDate = aggregate.EntryDate,
                Description = aggregate.Description,
                Status = aggregate.Status.ToString(),
                Deleted = aggregate.Deleted,
                LineItems = aggregate.LineItems.Select(item => new JournalEntryResponse.LineItem
                {
                    AccountCode = item.AccountCode,
                    DebitCredit = item.DebitCredit.ToString(),
                    Amount = item.Amount
                }).ToList()
            };

            return Ok(response);
        }
        catch (ArgumentException ex) when (ex.Message.Contains("見つかりません"))
        {
            return NotFound(new { message = ex.Message });
        }
    }

    /// <summary>
    /// 特定時点の仕訳状態を取得（タイムトラベル）
    /// </summary>
    [HttpGet("{id}/at")]
    [ProducesResponseType(typeof(JournalEntryResponse), StatusCodes.Status200OK)]
    [ProducesResponseType(StatusCodes.Status404NotFound)]
    public async Task<IActionResult> GetJournalEntryAt(
        string id,
        [FromQuery] DateTime pointInTime)
    {
        try
        {
            var aggregate = await _journalEntryService.GetJournalEntryAtAsync(id, pointInTime);

            var response = new JournalEntryResponse
            {
                Id = aggregate.Id,
                EntryDate = aggregate.EntryDate,
                Description = aggregate.Description,
                Status = aggregate.Status.ToString(),
                Deleted = aggregate.Deleted,
                LineItems = aggregate.LineItems.Select(item => new JournalEntryResponse.LineItem
                {
                    AccountCode = item.AccountCode,
                    DebitCredit = item.DebitCredit.ToString(),
                    Amount = item.Amount
                }).ToList()
            };

            return Ok(response);
        }
        catch (ArgumentException ex) when (ex.Message.Contains("見つかりません"))
        {
            return NotFound(new { message = ex.Message });
        }
    }
}

// リクエスト/レスポンス DTO

/// <summary>
/// 仕訳作成リクエスト
/// </summary>
public class CreateJournalEntryRequest
{
    /// <summary>
    /// 仕訳日
    /// </summary>
    public required DateOnly EntryDate { get; set; }

    /// <summary>
    /// 摘要
    /// </summary>
    public required string Description { get; set; }

    /// <summary>
    /// 明細リスト
    /// </summary>
    public required List<LineItemRequest> LineItems { get; set; }

    /// <summary>
    /// ユーザーID
    /// </summary>
    public required string UserId { get; set; }
}

/// <summary>
/// 仕訳明細リクエスト
/// </summary>
public class LineItemRequest
{
    /// <summary>
    /// 勘定科目コード
    /// </summary>
    public required string AccountCode { get; set; }

    /// <summary>
    /// 貸借区分（DEBIT, CREDIT）
    /// </summary>
    public required string DebitCredit { get; set; }

    /// <summary>
    /// 金額
    /// </summary>
    public required decimal Amount { get; set; }
}

/// <summary>
/// 仕訳作成レスポンス
/// </summary>
public record CreateJournalEntryResponse(string Id);

/// <summary>
/// 仕訳承認リクエスト
/// </summary>
public class ApproveJournalEntryRequest
{
    /// <summary>
    /// 承認者ID
    /// </summary>
    public required string ApprovedBy { get; set; }

    /// <summary>
    /// 承認コメント
    /// </summary>
    public required string Comment { get; set; }
}

/// <summary>
/// 仕訳削除リクエスト
/// </summary>
public class DeleteJournalEntryRequest
{
    /// <summary>
    /// 削除理由
    /// </summary>
    public required string Reason { get; set; }

    /// <summary>
    /// ユーザーID
    /// </summary>
    public required string UserId { get; set; }
}

/// <summary>
/// 仕訳レスポンス
/// </summary>
public class JournalEntryResponse
{
    /// <summary>
    /// 仕訳ID
    /// </summary>
    public required string Id { get; set; }

    /// <summary>
    /// 仕訳日
    /// </summary>
    public required DateOnly EntryDate { get; set; }

    /// <summary>
    /// 摘要
    /// </summary>
    public required string Description { get; set; }

    /// <summary>
    /// ステータス
    /// </summary>
    public required string Status { get; set; }

    /// <summary>
    /// 削除済みフラグ
    /// </summary>
    public required bool Deleted { get; set; }

    /// <summary>
    /// 明細リスト
    /// </summary>
    public required List<LineItem> LineItems { get; set; }

    /// <summary>
    /// 仕訳明細
    /// </summary>
    public class LineItem
    {
        /// <summary>
        /// 勘定科目コード
        /// </summary>
        public required string AccountCode { get; set; }

        /// <summary>
        /// 貸借区分
        /// </summary>
        public required string DebitCredit { get; set; }

        /// <summary>
        /// 金額
        /// </summary>
        public required decimal Amount { get; set; }
    }
}
