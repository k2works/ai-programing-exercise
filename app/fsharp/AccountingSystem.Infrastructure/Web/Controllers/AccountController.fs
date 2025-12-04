namespace AccountingSystem.Infrastructure.Web.Controllers

open System.Threading.Tasks
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Mvc
open AccountingSystem.Application.Services
open AccountingSystem.Infrastructure.Web.Dtos

/// <summary>
/// 勘定科目 REST API コントローラー（Input Adapter）
/// </summary>
[<ApiController>]
[<Route("api/v1/accounts")>]
[<Tags("勘定科目API")>]
type AccountController(accountService: IAccountService) =
    inherit ControllerBase()

    /// <summary>
    /// すべての勘定科目を取得
    /// </summary>
    [<HttpGet>]
    [<ProducesResponseType(typeof<AccountResponse list>, StatusCodes.Status200OK)>]
    member _.GetAllAccounts() : Task<IActionResult> =
        task {
            let! accounts = accountService.GetAllAccountsAsync()
            let response = accounts |> List.map AccountResponse.from
            return OkObjectResult(response) :> IActionResult
        }

    /// <summary>
    /// 科目コードで勘定科目を取得
    /// </summary>
    [<HttpGet("{accountCode}")>]
    [<ProducesResponseType(typeof<AccountResponse>, StatusCodes.Status200OK)>]
    [<ProducesResponseType(typeof<ErrorResponse>, StatusCodes.Status404NotFound)>]
    member _.GetAccount(accountCode: string) : Task<IActionResult> =
        task {
            let! account = accountService.GetAccountByCodeAsync(accountCode)
            return OkObjectResult(AccountResponse.from account) :> IActionResult
        }

    /// <summary>
    /// 勘定科目種別で勘定科目を取得
    /// </summary>
    [<HttpGet("type/{accountType}")>]
    [<ProducesResponseType(typeof<AccountResponse list>, StatusCodes.Status200OK)>]
    [<ProducesResponseType(typeof<ErrorResponse>, StatusCodes.Status400BadRequest)>]
    member _.GetAccountsByType(accountType: string) : Task<IActionResult> =
        task {
            let! accounts = accountService.GetAccountsByTypeAsync(accountType)
            let response = accounts |> List.map AccountResponse.from
            return OkObjectResult(response) :> IActionResult
        }

    /// <summary>
    /// 新しい勘定科目を作成
    /// </summary>
    [<HttpPost>]
    [<ProducesResponseType(typeof<AccountResponse>, StatusCodes.Status201Created)>]
    [<ProducesResponseType(typeof<ErrorResponse>, StatusCodes.Status400BadRequest)>]
    [<ProducesResponseType(typeof<ErrorResponse>, StatusCodes.Status409Conflict)>]
    member this.CreateAccount([<FromBody>] request: AccountRequest) : Task<IActionResult> =
        task {
            let account = AccountRequest.toDomain request
            let! created = accountService.CreateAccountAsync(account)

            return
                this.CreatedAtAction(
                    nameof this.GetAccount,
                    {| accountCode = created.AccountCode.Code |},
                    AccountResponse.from created) :> IActionResult
        }

    /// <summary>
    /// 勘定科目を更新
    /// </summary>
    [<HttpPut("{accountCode}")>]
    [<ProducesResponseType(typeof<AccountResponse>, StatusCodes.Status200OK)>]
    [<ProducesResponseType(typeof<ErrorResponse>, StatusCodes.Status404NotFound)>]
    [<ProducesResponseType(typeof<ErrorResponse>, StatusCodes.Status400BadRequest)>]
    member _.UpdateAccount(accountCode: string, [<FromBody>] request: AccountRequest) : Task<IActionResult> =
        task {
            let account = AccountRequest.toDomain request
            let! updated = accountService.UpdateAccountAsync accountCode account
            return OkObjectResult(AccountResponse.from updated) :> IActionResult
        }

    /// <summary>
    /// 勘定科目を削除
    /// </summary>
    [<HttpDelete("{accountCode}")>]
    [<ProducesResponseType(StatusCodes.Status204NoContent)>]
    [<ProducesResponseType(typeof<ErrorResponse>, StatusCodes.Status404NotFound)>]
    member _.DeleteAccount(accountCode: string) : Task<IActionResult> =
        task {
            do! accountService.DeleteAccountAsync(accountCode)
            return NoContentResult() :> IActionResult
        }
