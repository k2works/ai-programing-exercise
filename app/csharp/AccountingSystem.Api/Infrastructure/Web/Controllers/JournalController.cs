using AccountingSystem.Infrastructure.Web.Dtos;
using AccountingSystem.Application.Ports.In;
using Microsoft.AspNetCore.Mvc;

namespace AccountingSystem.Infrastructure.Web.Controllers;

/// <summary>
/// 仕訳 REST API コントローラー
/// </summary>
[ApiController]
[Route("api/v1/journals")]
[Tags("仕訳API")]
public class JournalController : ControllerBase
{
    private readonly IJournalService _journalService;

    public JournalController(IJournalService journalService)
    {
        _journalService = journalService;
    }

    /// <summary>
    /// 仕訳伝票番号で仕訳を取得
    /// </summary>
    /// <param name="journalNo">仕訳伝票番号</param>
    /// <returns>仕訳</returns>
    [HttpGet("{journalNo}")]
    [ProducesResponseType(typeof(JournalResponse), StatusCodes.Status200OK)]
    [ProducesResponseType(typeof(ErrorResponse), StatusCodes.Status404NotFound)]
    public async Task<IActionResult> GetByJournalNo(string journalNo)
    {
        var journal = await _journalService.GetJournalByNoAsync(journalNo);
        return Ok(JournalResponse.From(journal));
    }

    /// <summary>
    /// 新しい仕訳を作成
    /// </summary>
    /// <param name="request">仕訳作成リクエスト</param>
    /// <returns>作成された仕訳</returns>
    [HttpPost]
    [ProducesResponseType(typeof(JournalResponse), StatusCodes.Status201Created)]
    [ProducesResponseType(typeof(ErrorResponse), StatusCodes.Status400BadRequest)]
    [ProducesResponseType(typeof(ErrorResponse), StatusCodes.Status409Conflict)]
    public async Task<IActionResult> Create([FromBody] JournalRequest request)
    {
        var journal = JournalResponse.ToEntity(request);
        var created = await _journalService.CreateJournalAsync(journal);
        return CreatedAtAction(
            nameof(GetByJournalNo),
            new { journalNo = created.JournalNo },
            JournalResponse.From(created));
    }

    /// <summary>
    /// 仕訳を削除
    /// </summary>
    /// <param name="journalNo">仕訳伝票番号</param>
    /// <returns>なし</returns>
    [HttpDelete("{journalNo}")]
    [ProducesResponseType(StatusCodes.Status204NoContent)]
    [ProducesResponseType(typeof(ErrorResponse), StatusCodes.Status404NotFound)]
    public async Task<IActionResult> Delete(string journalNo)
    {
        await _journalService.DeleteJournalAsync(journalNo);
        return NoContent();
    }

    /// <summary>
    /// 仕訳の借方・貸方残高を検証
    /// </summary>
    /// <param name="journalNo">仕訳伝票番号</param>
    /// <returns>残高検証結果</returns>
    [HttpGet("{journalNo}/balance")]
    [ProducesResponseType(typeof(JournalBalanceResponse), StatusCodes.Status200OK)]
    [ProducesResponseType(typeof(ErrorResponse), StatusCodes.Status404NotFound)]
    public async Task<IActionResult> ValidateBalance(string journalNo)
    {
        var (debitTotal, creditTotal, isBalanced) = await _journalService.ValidateBalanceAsync(journalNo);
        return Ok(new JournalBalanceResponse
        {
            JournalNo = journalNo,
            DebitTotal = debitTotal,
            CreditTotal = creditTotal,
            IsBalanced = isBalanced
        });
    }
}
