namespace AccountingSystem.Application.Services

open System
open System.Threading.Tasks
open AccountingSystem.Domain.Models.AutoJournalPattern
open AccountingSystem.Application.Port.Out

/// <summary>
/// 自動仕訳サービスのエラー
/// </summary>
type AutoJournalError =
    | PatternNotFound of patternCode: string
    | PatternInactive of patternCode: string
    | NoPatternItems of patternId: int64
    | ExecutionFailed of message: string * detail: string

/// <summary>
/// 自動仕訳アプリケーションサービス
/// 自動仕訳パターンの管理と実行を担当
/// </summary>
type AutoJournalService(repository: IAutoJournalRepository) =

    /// <summary>
    /// 有効なパターン一覧を取得
    /// </summary>
    member this.GetActivePatternsAsync() : Task<AutoJournalPattern list> =
        task {
            let query = { PatternCode = None; IsActive = Some true }
            return! repository.GetPatternsAsync(query)
        }

    /// <summary>
    /// 全パターン一覧を取得
    /// </summary>
    member this.GetAllPatternsAsync() : Task<AutoJournalPattern list> =
        task {
            let query = { PatternCode = None; IsActive = None }
            return! repository.GetPatternsAsync(query)
        }

    /// <summary>
    /// パターンコードでパターンを取得
    /// </summary>
    member this.GetPatternByCodeAsync(patternCode: string) : Task<Result<AutoJournalPattern, AutoJournalError>> =
        task {
            let! pattern = repository.GetPatternByCodeAsync(patternCode)
            match pattern with
            | Some p -> return Ok p
            | None -> return Error (PatternNotFound patternCode)
        }

    /// <summary>
    /// パターンと明細を取得
    /// </summary>
    member this.GetPatternWithItemsAsync(patternCode: string) : Task<Result<AutoJournalPattern * AutoJournalPatternItem list, AutoJournalError>> =
        task {
            let! pattern = repository.GetPatternByCodeAsync(patternCode)
            match pattern with
            | Some p ->
                let! items = repository.GetPatternItemsAsync(p.Id)
                return Ok (p, items)
            | None ->
                return Error (PatternNotFound patternCode)
        }

    /// <summary>
    /// 新規パターンを登録
    /// </summary>
    member this.CreatePatternAsync(
        patternCode: string,
        patternName: string,
        sourceTableName: string,
        description: string option) : Task<AutoJournalPattern> =
        task {
            let pattern = {
                AutoJournalPattern.create patternCode patternName sourceTableName with
                    Description = description
            }
            return! repository.SavePatternAsync(pattern)
        }

    /// <summary>
    /// パターンを更新
    /// </summary>
    member this.UpdatePatternAsync(pattern: AutoJournalPattern) : Task<AutoJournalPattern> =
        task {
            return! repository.SavePatternAsync(pattern)
        }

    /// <summary>
    /// パターンを有効化
    /// </summary>
    member this.ActivatePatternAsync(patternCode: string) : Task<Result<AutoJournalPattern, AutoJournalError>> =
        task {
            let! pattern = repository.GetPatternByCodeAsync(patternCode)
            match pattern with
            | Some p ->
                let updated = { p with IsActive = true }
                let! saved = repository.SavePatternAsync(updated)
                return Ok saved
            | None ->
                return Error (PatternNotFound patternCode)
        }

    /// <summary>
    /// パターンを無効化
    /// </summary>
    member this.DeactivatePatternAsync(patternCode: string) : Task<Result<AutoJournalPattern, AutoJournalError>> =
        task {
            let! pattern = repository.GetPatternByCodeAsync(patternCode)
            match pattern with
            | Some p ->
                let updated = { p with IsActive = false }
                let! saved = repository.SavePatternAsync(updated)
                return Ok saved
            | None ->
                return Error (PatternNotFound patternCode)
        }

    /// <summary>
    /// パターン明細を設定
    /// </summary>
    member this.SetPatternItemsAsync(
        patternCode: string,
        items: AutoJournalPatternItem list) : Task<Result<unit, AutoJournalError>> =
        task {
            let! pattern = repository.GetPatternByCodeAsync(patternCode)
            match pattern with
            | Some p ->
                do! repository.SavePatternItemsAsync p.Id items
                return Ok ()
            | None ->
                return Error (PatternNotFound patternCode)
        }

    /// <summary>
    /// 自動仕訳を実行（検証のみ）
    /// パターンが有効で明細が存在するかを確認
    /// </summary>
    member this.ValidatePatternForExecutionAsync(patternCode: string) : Task<Result<AutoJournalPattern * AutoJournalPatternItem list, AutoJournalError>> =
        task {
            let! pattern = repository.GetPatternByCodeAsync(patternCode)
            match pattern with
            | None ->
                return Error (PatternNotFound patternCode)
            | Some p when not p.IsActive ->
                return Error (PatternInactive patternCode)
            | Some p ->
                let! items = repository.GetPatternItemsAsync(p.Id)
                if items.IsEmpty then
                    return Error (NoPatternItems p.Id)
                else
                    return Ok (p, items)
        }

    /// <summary>
    /// 実行ログを記録
    /// </summary>
    member this.LogExecutionAsync(result: AutoJournalExecutionResult) : Task<AutoJournalLog> =
        task {
            let log = {
                Id = 0L
                PatternId = result.PatternId
                ExecutedAt = DateTime.UtcNow
                ProcessedCount = result.ProcessedCount
                GeneratedCount = result.GeneratedCount
                Status = result.Status
                Message = result.Message
                ErrorDetail = result.ErrorDetail
                CreatedAt = DateTime.UtcNow
            }
            return! repository.SaveLogAsync(log)
        }

    /// <summary>
    /// 実行履歴を取得
    /// </summary>
    member this.GetExecutionHistoryAsync(patternCode: string, limit: int) : Task<Result<AutoJournalLog list, AutoJournalError>> =
        task {
            let! pattern = repository.GetPatternByCodeAsync(patternCode)
            match pattern with
            | Some p ->
                let! logs = repository.GetLogsByPatternIdAsync p.Id limit
                return Ok logs
            | None ->
                return Error (PatternNotFound patternCode)
        }
