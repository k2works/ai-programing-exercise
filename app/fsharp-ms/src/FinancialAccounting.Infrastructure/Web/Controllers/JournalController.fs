namespace FinancialAccounting.Infrastructure.Web.Controllers

open System.Threading.Tasks
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Mvc
open FinancialAccounting.Application.Ports.In
open FinancialAccounting.Infrastructure.Web.Dtos

/// <summary>
/// 仕訳 REST API コントローラー
/// </summary>
[<ApiController>]
[<Route("api/journals")>]
type JournalController(journalUseCase: IJournalUseCase) =
    inherit ControllerBase()

    /// <summary>
    /// 仕訳を作成
    /// </summary>
    [<HttpPost>]
    [<ProducesResponseType(typeof<JournalResponseDto>, StatusCodes.Status201Created)>]
    [<ProducesResponseType(StatusCodes.Status400BadRequest)>]
    member this.CreateJournal([<FromBody>] request: JournalRequestDto) : Task<IActionResult> =
        task {
            let createRequest = JournalDto.toCreateRequest request
            let! result = journalUseCase.CreateJournalAsync(createRequest)

            match result with
            | Ok journal ->
                let response = JournalDto.toResponse journal
                return this.CreatedAtAction(
                    nameof(this.GetJournal),
                    {| id = response.JournalId |},
                    response) :> IActionResult
            | Error msg ->
                return this.BadRequest({| error = msg |}) :> IActionResult
        }

    /// <summary>
    /// IDで仕訳を取得
    /// </summary>
    [<HttpGet("{id:int}")>]
    [<ProducesResponseType(typeof<JournalResponseDto>, StatusCodes.Status200OK)>]
    [<ProducesResponseType(StatusCodes.Status404NotFound)>]
    member this.GetJournal(id: int) : Task<IActionResult> =
        task {
            let! journal = journalUseCase.GetJournalByIdAsync(id)

            match journal with
            | Some j ->
                return this.Ok(JournalDto.toResponse j) :> IActionResult
            | None ->
                return this.NotFound({| error = $"仕訳ID {id} が見つかりません" |}) :> IActionResult
        }

    /// <summary>
    /// 会計年度で仕訳一覧を取得
    /// </summary>
    [<HttpGet>]
    [<ProducesResponseType(typeof<JournalResponseDto[]>, StatusCodes.Status200OK)>]
    member this.GetJournalsByFiscalYear([<FromQuery>] fiscalYear: int) : Task<IActionResult> =
        task {
            let! journals = journalUseCase.GetJournalsByFiscalYearAsync(fiscalYear)
            let response = journals |> List.map JournalDto.toResponse |> List.toArray
            return this.Ok(response) :> IActionResult
        }
