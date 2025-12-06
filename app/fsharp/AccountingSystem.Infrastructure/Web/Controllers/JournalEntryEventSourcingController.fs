namespace AccountingSystem.Infrastructure.Web.Controllers

open System
open System.Threading.Tasks
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Mvc
open AccountingSystem.Application.Port.In
open AccountingSystem.Infrastructure.Web.Dtos

/// <summary>
/// 仕訳イベントソーシング REST API コントローラー（Input Adapter）
/// </summary>
[<ApiController>]
[<Route("api/v1/journal-entries")>]
[<Tags("仕訳イベントソーシングAPI")>]
type JournalEntryEventSourcingController(journalEntryService: IJournalEntryEventSourcingUseCase) =
    inherit ControllerBase()

    /// <summary>
    /// 仕訳を取得
    /// </summary>
    [<HttpGet("{id}")>]
    [<ProducesResponseType(typeof<JournalEntryResponse>, StatusCodes.Status200OK)>]
    [<ProducesResponseType(typeof<ErrorResponse>, StatusCodes.Status404NotFound)>]
    member this.GetJournalEntry(id: string) : Task<IActionResult> =
        task {
            let! aggregate = journalEntryService.GetJournalEntryAsync(id)

            match aggregate with
            | Some a when not a.Deleted ->
                return this.Ok(JournalEntryResponse.from a) :> IActionResult
            | _ ->
                return this.NotFound(ErrorResponse.create 404 "NotFound" "仕訳が見つかりません") :> IActionResult
        }

    /// <summary>
    /// すべての仕訳を取得
    /// </summary>
    [<HttpGet>]
    [<ProducesResponseType(typeof<JournalEntryResponse list>, StatusCodes.Status200OK)>]
    member this.GetAllJournalEntries() : Task<IActionResult> =
        task {
            let! aggregates = journalEntryService.GetAllJournalEntriesAsync()
            let response = JournalEntryResponse.fromList aggregates
            return this.Ok(response) :> IActionResult
        }

    /// <summary>
    /// 特定時点の仕訳を取得（タイムトラベル）
    /// </summary>
    [<HttpGet("{id}/at")>]
    [<ProducesResponseType(typeof<JournalEntryResponse>, StatusCodes.Status200OK)>]
    [<ProducesResponseType(typeof<ErrorResponse>, StatusCodes.Status404NotFound)>]
    member this.GetJournalEntryAt(id: string, [<FromQuery>] pointInTime: DateTime) : Task<IActionResult> =
        task {
            let! aggregate = journalEntryService.GetJournalEntryAtAsync id pointInTime

            match aggregate with
            | Some a ->
                return this.Ok(JournalEntryResponse.from a) :> IActionResult
            | None ->
                return this.NotFound(ErrorResponse.create 404 "NotFound" "指定時点の仕訳が見つかりません") :> IActionResult
        }

    /// <summary>
    /// 新しい仕訳を作成
    /// </summary>
    [<HttpPost>]
    [<ProducesResponseType(typeof<JournalEntryResponse>, StatusCodes.Status201Created)>]
    [<ProducesResponseType(typeof<ErrorResponse>, StatusCodes.Status400BadRequest)>]
    [<ProducesResponseType(typeof<ErrorResponse>, StatusCodes.Status422UnprocessableEntity)>]
    member this.CreateJournalEntry([<FromBody>] request: CreateJournalEntryRequest) : Task<IActionResult> =
        task {
            let lineItems =
                request.LineItems
                |> Array.toList
                |> List.map JournalEntryLineItemRequest.toDomain

            // TODO: ユーザー認証からユーザーIDを取得
            let userId = "system"

            let! result =
                journalEntryService.CreateJournalEntryAsync
                    request.Id
                    request.EntryDate
                    request.Description
                    lineItems
                    userId

            match result with
            | Ok aggregate ->
                return
                    this.CreatedAtAction(
                        nameof this.GetJournalEntry,
                        {| id = aggregate.Id |},
                        JournalEntryResponse.from aggregate) :> IActionResult
            | Error msg ->
                return this.UnprocessableEntity(ErrorResponse.create 422 "UnprocessableEntity" msg) :> IActionResult
        }

    /// <summary>
    /// 仕訳を承認
    /// </summary>
    [<HttpPost("{id}/approve")>]
    [<ProducesResponseType(typeof<JournalEntryResponse>, StatusCodes.Status200OK)>]
    [<ProducesResponseType(typeof<ErrorResponse>, StatusCodes.Status404NotFound)>]
    [<ProducesResponseType(typeof<ErrorResponse>, StatusCodes.Status400BadRequest)>]
    [<ProducesResponseType(typeof<ErrorResponse>, StatusCodes.Status422UnprocessableEntity)>]
    member this.ApproveJournalEntry(id: string, [<FromBody>] request: ApproveJournalEntryRequest) : Task<IActionResult> =
        task {
            let comment =
                if String.IsNullOrEmpty(request.ApprovalComment) then ""
                else request.ApprovalComment

            let! result =
                journalEntryService.ApproveJournalEntryAsync
                    id
                    request.ApprovedBy
                    comment

            match result with
            | Ok aggregate ->
                return this.Ok(JournalEntryResponse.from aggregate) :> IActionResult
            | Error msg when msg.Contains("見つかりません") ->
                return this.NotFound(ErrorResponse.create 404 "NotFound" msg) :> IActionResult
            | Error msg ->
                return this.UnprocessableEntity(ErrorResponse.create 422 "UnprocessableEntity" msg) :> IActionResult
        }

    /// <summary>
    /// 仕訳を削除
    /// </summary>
    [<HttpDelete("{id}")>]
    [<ProducesResponseType(StatusCodes.Status204NoContent)>]
    [<ProducesResponseType(typeof<ErrorResponse>, StatusCodes.Status404NotFound)>]
    [<ProducesResponseType(typeof<ErrorResponse>, StatusCodes.Status422UnprocessableEntity)>]
    member this.DeleteJournalEntry(id: string, [<FromBody>] request: DeleteJournalEntryRequest) : Task<IActionResult> =
        task {
            // TODO: ユーザー認証からユーザーIDを取得
            let userId = "system"

            let! result =
                journalEntryService.DeleteJournalEntryAsync
                    id
                    request.Reason
                    userId

            match result with
            | Ok _ ->
                return this.NoContent() :> IActionResult
            | Error msg when msg.Contains("見つかりません") ->
                return this.NotFound(ErrorResponse.create 404 "NotFound" msg) :> IActionResult
            | Error msg ->
                return this.UnprocessableEntity(ErrorResponse.create 422 "UnprocessableEntity" msg) :> IActionResult
        }
