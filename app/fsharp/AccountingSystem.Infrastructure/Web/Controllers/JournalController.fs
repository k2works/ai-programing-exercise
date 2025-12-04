namespace AccountingSystem.Infrastructure.Web.Controllers

open System
open System.Threading.Tasks
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Mvc
open AccountingSystem.Application.Port.In
open AccountingSystem.Infrastructure.Web.Dtos

/// <summary>
/// 仕訳 REST API コントローラー（Input Adapter）
/// </summary>
[<ApiController>]
[<Route("api/v1/journals")>]
[<Tags("仕訳API")>]
type JournalController(journalService: IJournalUseCase) =
    inherit ControllerBase()

    /// <summary>
    /// 伝票番号で仕訳を取得
    /// </summary>
    [<HttpGet("{voucherNumber:int}")>]
    [<ProducesResponseType(typeof<JournalResponse>, StatusCodes.Status200OK)>]
    [<ProducesResponseType(typeof<ErrorResponse>, StatusCodes.Status404NotFound)>]
    member _.GetJournal(voucherNumber: int) : Task<IActionResult> =
        task {
            let! journal = journalService.GetJournalByVoucherNumberAsync(voucherNumber)
            return OkObjectResult(JournalResponse.from journal) :> IActionResult
        }

    /// <summary>
    /// 日付範囲で仕訳を取得
    /// </summary>
    [<HttpGet>]
    [<ProducesResponseType(typeof<JournalResponse list>, StatusCodes.Status200OK)>]
    [<ProducesResponseType(typeof<ErrorResponse>, StatusCodes.Status400BadRequest)>]
    member _.GetJournalsByDateRange(
        [<FromQuery>] fromDate: DateTime,
        [<FromQuery>] toDate: DateTime) : Task<IActionResult> =
        task {
            let! journals = journalService.GetJournalsByDateRangeAsync(fromDate, toDate)
            let response = journals |> List.map JournalResponse.from
            return OkObjectResult(response) :> IActionResult
        }

    /// <summary>
    /// 新しい仕訳を作成
    /// </summary>
    [<HttpPost>]
    [<ProducesResponseType(typeof<JournalResponse>, StatusCodes.Status201Created)>]
    [<ProducesResponseType(typeof<ErrorResponse>, StatusCodes.Status400BadRequest)>]
    [<ProducesResponseType(typeof<ErrorResponse>, StatusCodes.Status422UnprocessableEntity)>]
    member this.CreateJournal([<FromBody>] request: JournalRequest) : Task<IActionResult> =
        task {
            let journal = JournalRequest.toDomain request
            let! created = journalService.CreateJournalAsync(journal)

            return
                this.CreatedAtAction(
                    nameof this.GetJournal,
                    {| voucherNumber = created.VoucherNumber.Number |},
                    JournalResponse.from created) :> IActionResult
        }

    /// <summary>
    /// 仕訳を更新
    /// </summary>
    [<HttpPut("{voucherNumber:int}")>]
    [<ProducesResponseType(typeof<JournalResponse>, StatusCodes.Status200OK)>]
    [<ProducesResponseType(typeof<ErrorResponse>, StatusCodes.Status404NotFound)>]
    [<ProducesResponseType(typeof<ErrorResponse>, StatusCodes.Status400BadRequest)>]
    member _.UpdateJournal(voucherNumber: int, [<FromBody>] request: JournalRequest) : Task<IActionResult> =
        task {
            let journal = JournalRequest.toDomain request
            let! updated = journalService.UpdateJournalAsync voucherNumber journal
            return OkObjectResult(JournalResponse.from updated) :> IActionResult
        }

    /// <summary>
    /// 仕訳を削除
    /// </summary>
    [<HttpDelete("{voucherNumber:int}")>]
    [<ProducesResponseType(StatusCodes.Status204NoContent)>]
    [<ProducesResponseType(typeof<ErrorResponse>, StatusCodes.Status404NotFound)>]
    member _.DeleteJournal(voucherNumber: int) : Task<IActionResult> =
        task {
            do! journalService.DeleteJournalAsync(voucherNumber)
            return NoContentResult() :> IActionResult
        }
