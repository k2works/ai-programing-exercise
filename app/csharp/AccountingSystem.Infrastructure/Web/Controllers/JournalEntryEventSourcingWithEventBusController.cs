namespace AccountingSystem.Infrastructure.Web.Controllers;

using AccountingSystem.Application.Ports.In;
using AccountingSystem.Application.Services;
using Microsoft.AspNetCore.Http;
using Microsoft.AspNetCore.Mvc;

/// <summary>
/// イベントソーシング版仕訳 API コントローラー（イベントバス連携）
/// RabbitMQ へのイベント発行機能付き
/// </summary>
[ApiController]
[Route("api/v1/journal-entries-es-eventbus")]
[Tags("Journal Entry Event Sourcing with EventBus")]
public class JournalEntryEventSourcingWithEventBusController : ControllerBase
{
    private readonly JournalEntryEventSourcingServiceWithEventBus _journalEntryService;

    public JournalEntryEventSourcingWithEventBusController(
        JournalEntryEventSourcingServiceWithEventBus journalEntryService)
    {
        _journalEntryService = journalEntryService;
    }

    /// <summary>
    /// 仕訳を作成する（イベントソーシング + イベントバス）
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
