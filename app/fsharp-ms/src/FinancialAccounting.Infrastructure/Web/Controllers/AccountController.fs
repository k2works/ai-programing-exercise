namespace FinancialAccounting.Infrastructure.Web.Controllers

open System.Threading.Tasks
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Mvc
open FinancialAccounting.Application.Ports.In
open FinancialAccounting.Infrastructure.Web.Dtos

/// <summary>
/// 勘定科目 REST API コントローラー
/// </summary>
[<ApiController>]
[<Route("api/accounts")>]
type AccountController(accountUseCase: IAccountUseCase) =
    inherit ControllerBase()

    /// <summary>
    /// 勘定科目を作成
    /// </summary>
    [<HttpPost>]
    [<ProducesResponseType(typeof<AccountResponseDto>, StatusCodes.Status201Created)>]
    [<ProducesResponseType(StatusCodes.Status400BadRequest)>]
    member this.CreateAccount([<FromBody>] request: AccountRequestDto) : Task<IActionResult> =
        task {
            let createRequest = AccountDto.toCreateRequest request
            let! result = accountUseCase.CreateAccountAsync(createRequest)

            match result with
            | Ok account ->
                let response = AccountDto.toResponse account
                return this.CreatedAtAction(
                    nameof(this.GetAccount),
                    {| id = response.AccountId |},
                    response) :> IActionResult
            | Error msg ->
                return this.BadRequest({| error = msg |}) :> IActionResult
        }

    /// <summary>
    /// IDで勘定科目を取得
    /// </summary>
    [<HttpGet("{id:int}")>]
    [<ProducesResponseType(typeof<AccountResponseDto>, StatusCodes.Status200OK)>]
    [<ProducesResponseType(StatusCodes.Status404NotFound)>]
    member this.GetAccount(id: int) : Task<IActionResult> =
        task {
            let! account = accountUseCase.GetAccountByIdAsync(id)

            match account with
            | Some a ->
                return this.Ok(AccountDto.toResponse a) :> IActionResult
            | None ->
                return this.NotFound({| error = $"勘定科目ID {id} が見つかりません" |}) :> IActionResult
        }

    /// <summary>
    /// 勘定科目コードで勘定科目を取得
    /// </summary>
    [<HttpGet("code/{code}")>]
    [<ProducesResponseType(typeof<AccountResponseDto>, StatusCodes.Status200OK)>]
    [<ProducesResponseType(StatusCodes.Status404NotFound)>]
    member this.GetAccountByCode(code: string) : Task<IActionResult> =
        task {
            let! account = accountUseCase.GetAccountByCodeAsync(code)

            match account with
            | Some a ->
                return this.Ok(AccountDto.toResponse a) :> IActionResult
            | None ->
                return this.NotFound({| error = $"勘定科目コード {code} が見つかりません" |}) :> IActionResult
        }

    /// <summary>
    /// 全ての勘定科目を取得
    /// </summary>
    [<HttpGet>]
    [<ProducesResponseType(typeof<AccountResponseDto[]>, StatusCodes.Status200OK)>]
    member this.GetAllAccounts() : Task<IActionResult> =
        task {
            let! accounts = accountUseCase.GetAllAccountsAsync()
            let response = accounts |> List.map AccountDto.toResponse |> List.toArray
            return this.Ok(response) :> IActionResult
        }

    /// <summary>
    /// 勘定科目種別で勘定科目を取得
    /// </summary>
    [<HttpGet("type/{accountType}")>]
    [<ProducesResponseType(typeof<AccountResponseDto[]>, StatusCodes.Status200OK)>]
    [<ProducesResponseType(StatusCodes.Status400BadRequest)>]
    member this.GetAccountsByType(accountType: string) : Task<IActionResult> =
        task {
            let! result = accountUseCase.GetAccountsByTypeAsync(accountType)

            match result with
            | Ok accounts ->
                let response = accounts |> List.map AccountDto.toResponse |> List.toArray
                return this.Ok(response) :> IActionResult
            | Error msg ->
                return this.BadRequest({| error = msg |}) :> IActionResult
        }

    /// <summary>
    /// 勘定科目を更新
    /// </summary>
    [<HttpPut("{id:int}")>]
    [<ProducesResponseType(typeof<AccountResponseDto>, StatusCodes.Status200OK)>]
    [<ProducesResponseType(StatusCodes.Status400BadRequest)>]
    [<ProducesResponseType(StatusCodes.Status404NotFound)>]
    member this.UpdateAccount(id: int, [<FromBody>] request: AccountUpdateRequestDto) : Task<IActionResult> =
        task {
            let updateRequest = AccountDto.toUpdateRequest id request
            let! result = accountUseCase.UpdateAccountAsync(updateRequest)

            match result with
            | Ok account ->
                return this.Ok(AccountDto.toResponse account) :> IActionResult
            | Error msg when msg.Contains("見つかりません") ->
                return this.NotFound({| error = msg |}) :> IActionResult
            | Error msg ->
                return this.BadRequest({| error = msg |}) :> IActionResult
        }

    /// <summary>
    /// 勘定科目を削除
    /// </summary>
    [<HttpDelete("{id:int}")>]
    [<ProducesResponseType(StatusCodes.Status204NoContent)>]
    [<ProducesResponseType(StatusCodes.Status404NotFound)>]
    member this.DeleteAccount(id: int) : Task<IActionResult> =
        task {
            let! result = accountUseCase.DeleteAccountAsync(id)

            match result with
            | Ok _ ->
                return this.NoContent() :> IActionResult
            | Error msg ->
                return this.NotFound({| error = msg |}) :> IActionResult
        }
