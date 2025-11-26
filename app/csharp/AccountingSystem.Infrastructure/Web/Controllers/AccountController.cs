using Microsoft.AspNetCore.Http;
using AccountingSystem.Infrastructure.Web.Dtos;
using AccountingSystem.Application.Ports.In;
using Microsoft.AspNetCore.Mvc;

namespace AccountingSystem.Infrastructure.Web.Controllers;

/// <summary>
/// 勘定科目 REST API コントローラー
/// </summary>
[ApiController]
[Route("api/v1/accounts")]
[Tags("勘定科目API")]
public class AccountController : ControllerBase
{
    private readonly IAccountService _accountService;

    public AccountController(IAccountService accountService)
    {
        _accountService = accountService;
    }

    /// <summary>
    /// すべての勘定科目を取得
    /// </summary>
    /// <returns>勘定科目一覧</returns>
    [HttpGet]
    [ProducesResponseType(typeof(IEnumerable<AccountResponse>), StatusCodes.Status200OK)]
    public async Task<IActionResult> GetAll()
    {
        var accounts = await _accountService.GetAllAccountsAsync();
        return Ok(accounts.Select(AccountResponse.From));
    }

    /// <summary>
    /// 科目コードで勘定科目を取得
    /// </summary>
    /// <param name="accountCode">勘定科目コード</param>
    /// <returns>勘定科目</returns>
    [HttpGet("{accountCode}")]
    [ProducesResponseType(typeof(AccountResponse), StatusCodes.Status200OK)]
    [ProducesResponseType(typeof(ErrorResponse), StatusCodes.Status404NotFound)]
    public async Task<IActionResult> GetByCode(string accountCode)
    {
        var account = await _accountService.GetAccountByCodeAsync(accountCode);
        return Ok(AccountResponse.From(account));
    }

    /// <summary>
    /// BSPL区分で勘定科目を取得
    /// </summary>
    /// <param name="bsplType">BSPL区分（B: 貸借対照表、P: 損益計算書）</param>
    /// <returns>勘定科目一覧</returns>
    [HttpGet("by-bspl-type/{bsplType}")]
    [ProducesResponseType(typeof(IEnumerable<AccountResponse>), StatusCodes.Status200OK)]
    [ProducesResponseType(typeof(ErrorResponse), StatusCodes.Status400BadRequest)]
    public async Task<IActionResult> GetByBsplType(string bsplType)
    {
        var accounts = await _accountService.GetAccountsByBsplTypeAsync(bsplType);
        return Ok(accounts.Select(AccountResponse.From));
    }

    /// <summary>
    /// 勘定科目種別で勘定科目を取得
    /// </summary>
    /// <param name="accountType">勘定科目種別</param>
    /// <returns>勘定科目一覧</returns>
    [HttpGet("by-type/{accountType}")]
    [ProducesResponseType(typeof(IEnumerable<AccountResponse>), StatusCodes.Status200OK)]
    public async Task<IActionResult> GetByType(string accountType)
    {
        var accounts = await _accountService.GetAccountsByTypeAsync(accountType);
        return Ok(accounts.Select(AccountResponse.From));
    }

    /// <summary>
    /// 新しい勘定科目を作成
    /// </summary>
    /// <param name="request">勘定科目作成リクエスト</param>
    /// <returns>作成された勘定科目</returns>
    [HttpPost]
    [ProducesResponseType(typeof(AccountResponse), StatusCodes.Status201Created)]
    [ProducesResponseType(typeof(ErrorResponse), StatusCodes.Status400BadRequest)]
    [ProducesResponseType(typeof(ErrorResponse), StatusCodes.Status409Conflict)]
    public async Task<IActionResult> Create([FromBody] AccountRequest request)
    {
        var account = AccountResponse.ToEntity(request);
        var created = await _accountService.CreateAccountAsync(account);
        return CreatedAtAction(
            nameof(GetByCode),
            new { accountCode = created.AccountCode },
            AccountResponse.From(created));
    }

    /// <summary>
    /// 勘定科目を更新
    /// </summary>
    /// <param name="accountCode">勘定科目コード</param>
    /// <param name="request">勘定科目更新リクエスト</param>
    /// <returns>更新された勘定科目</returns>
    [HttpPut("{accountCode}")]
    [ProducesResponseType(typeof(AccountResponse), StatusCodes.Status200OK)]
    [ProducesResponseType(typeof(ErrorResponse), StatusCodes.Status400BadRequest)]
    [ProducesResponseType(typeof(ErrorResponse), StatusCodes.Status404NotFound)]
    public async Task<IActionResult> Update(string accountCode, [FromBody] AccountRequest request)
    {
        var account = AccountResponse.ToEntity(request);
        var updated = await _accountService.UpdateAccountAsync(accountCode, account);
        return Ok(AccountResponse.From(updated));
    }

    /// <summary>
    /// 勘定科目を削除
    /// </summary>
    /// <param name="accountCode">勘定科目コード</param>
    /// <returns>なし</returns>
    [HttpDelete("{accountCode}")]
    [ProducesResponseType(StatusCodes.Status204NoContent)]
    [ProducesResponseType(typeof(ErrorResponse), StatusCodes.Status404NotFound)]
    public async Task<IActionResult> Delete(string accountCode)
    {
        await _accountService.DeleteAccountAsync(accountCode);
        return NoContent();
    }
}
