namespace AccountingSystem.Tests.Application

open System
open System.Threading.Tasks
open Xunit
open FsUnit.Xunit
open AccountingSystem.Domain.Models.AutoJournalPattern
open AccountingSystem.Domain.Types
open AccountingSystem.Application.Port.Out
open AccountingSystem.Application.Services

/// <summary>
/// IAutoJournalRepository のモック実装
/// </summary>
type MockAutoJournalRepository() =
    let mutable patterns: AutoJournalPattern list = []
    let mutable patternItems: Map<int64, AutoJournalPatternItem list> = Map.empty
    let mutable logs: AutoJournalLog list = []
    let mutable nextPatternId = 1L
    let mutable nextLogId = 1L

    /// テストデータをセットアップ
    member this.SetupPatterns(data: AutoJournalPattern list) =
        patterns <- data

    /// テストデータをセットアップ（明細付き）
    member this.SetupPatternItems(patternId: int64, items: AutoJournalPatternItem list) =
        patternItems <- patternItems.Add(patternId, items)

    /// 保存されたパターンを取得
    member this.SavedPatterns = patterns

    /// 保存されたログを取得
    member this.SavedLogs = logs

    interface IAutoJournalRepository with
        member this.GetPatternsAsync(query: AutoJournalPatternQuery) : Task<AutoJournalPattern list> =
            let filtered =
                patterns
                |> List.filter (fun p ->
                    match query.PatternCode with
                    | Some code -> p.PatternCode = code
                    | None -> true)
                |> List.filter (fun p ->
                    match query.IsActive with
                    | Some active -> p.IsActive = active
                    | None -> true)
            Task.FromResult(filtered)

        member this.GetPatternByCodeAsync(patternCode: string) : Task<AutoJournalPattern option> =
            let result = patterns |> List.tryFind (fun p -> p.PatternCode = patternCode)
            Task.FromResult(result)

        member this.GetPatternItemsAsync(patternId: int64) : Task<AutoJournalPatternItem list> =
            let items = patternItems |> Map.tryFind patternId |> Option.defaultValue []
            Task.FromResult(items)

        member this.SavePatternAsync(pattern: AutoJournalPattern) : Task<AutoJournalPattern> =
            let savedPattern =
                if pattern.Id = 0L then
                    let newPattern = { pattern with Id = nextPatternId }
                    nextPatternId <- nextPatternId + 1L
                    patterns <- patterns @ [newPattern]
                    newPattern
                else
                    patterns <- patterns |> List.map (fun p -> if p.Id = pattern.Id then pattern else p)
                    pattern
            Task.FromResult(savedPattern)

        member this.SavePatternItemsAsync (patternId: int64) (items: AutoJournalPatternItem list) : Task<unit> =
            patternItems <- patternItems.Add(patternId, items)
            Task.FromResult(())

        member this.SaveLogAsync(log: AutoJournalLog) : Task<AutoJournalLog> =
            let savedLog = { log with Id = nextLogId }
            nextLogId <- nextLogId + 1L
            logs <- logs @ [savedLog]
            Task.FromResult(savedLog)

        member this.GetLogsByPatternIdAsync (patternId: int64) (limit: int) : Task<AutoJournalLog list> =
            let result =
                logs
                |> List.filter (fun l -> l.PatternId = patternId)
                |> List.sortByDescending (fun l -> l.ExecutedAt)
                |> List.truncate limit
            Task.FromResult(result)

/// <summary>
/// AutoJournalService のテスト
/// </summary>
type AutoJournalServiceTest() =

    let createTestPattern id code name isActive =
        { Id = id
          PatternCode = code
          PatternName = name
          SourceTableName = "テストテーブル"
          Description = None
          IsActive = isActive }

    let createTestItem patternId lineNo dcType accountCode =
        { Id = 0L
          PatternId = patternId
          LineNumber = lineNo
          DebitCreditType = dcType
          AccountCode = AccountCode.Create(accountCode)
          AmountExpression = "{金額}"
          DescriptionTemplate = None }

    [<Fact>]
    member this.``GetActivePatternsAsync で有効なパターンのみ取得できる``() =
        task {
            // Given
            let mockRepo = MockAutoJournalRepository()
            mockRepo.SetupPatterns([
                createTestPattern 1L "SALES001" "売上計上" true
                createTestPattern 2L "SALES002" "売上取消" false
                createTestPattern 3L "PURCHASE001" "仕入計上" true
            ])
            let service = AutoJournalService(mockRepo)

            // When
            let! patterns = service.GetActivePatternsAsync()

            // Then
            patterns.Length |> should equal 2
            patterns |> List.forall (fun p -> p.IsActive) |> should equal true
        }

    [<Fact>]
    member this.``GetAllPatternsAsync で全パターンを取得できる``() =
        task {
            // Given
            let mockRepo = MockAutoJournalRepository()
            mockRepo.SetupPatterns([
                createTestPattern 1L "SALES001" "売上計上" true
                createTestPattern 2L "SALES002" "売上取消" false
            ])
            let service = AutoJournalService(mockRepo)

            // When
            let! patterns = service.GetAllPatternsAsync()

            // Then
            patterns.Length |> should equal 2
        }

    [<Fact>]
    member this.``GetPatternByCodeAsync で存在するパターンを取得できる``() =
        task {
            // Given
            let mockRepo = MockAutoJournalRepository()
            mockRepo.SetupPatterns([createTestPattern 1L "SALES001" "売上計上" true])
            let service = AutoJournalService(mockRepo)

            // When
            let! result = service.GetPatternByCodeAsync("SALES001")

            // Then
            match result with
            | Ok pattern ->
                pattern.PatternCode |> should equal "SALES001"
                pattern.PatternName |> should equal "売上計上"
            | Error _ -> failwith "パターンが取得できるはず"
        }

    [<Fact>]
    member this.``GetPatternByCodeAsync で存在しないパターンはエラーを返す``() =
        task {
            // Given
            let mockRepo = MockAutoJournalRepository()
            let service = AutoJournalService(mockRepo)

            // When
            let! result = service.GetPatternByCodeAsync("NOTFOUND")

            // Then
            match result with
            | Ok _ -> failwith "エラーが返るはず"
            | Error e ->
                match e with
                | PatternNotFound code -> code |> should equal "NOTFOUND"
                | _ -> failwith "PatternNotFound エラーのはず"
        }

    [<Fact>]
    member this.``GetPatternWithItemsAsync でパターンと明細を取得できる``() =
        task {
            // Given
            let mockRepo = MockAutoJournalRepository()
            mockRepo.SetupPatterns([createTestPattern 1L "SALES001" "売上計上" true])
            mockRepo.SetupPatternItems(1L, [
                createTestItem 1L 1 Debit "130"
                createTestItem 1L 2 Credit "500"
            ])
            let service = AutoJournalService(mockRepo)

            // When
            let! result = service.GetPatternWithItemsAsync("SALES001")

            // Then
            match result with
            | Ok (pattern, items) ->
                pattern.PatternCode |> should equal "SALES001"
                items.Length |> should equal 2
            | Error _ -> failwith "パターンと明細が取得できるはず"
        }

    [<Fact>]
    member this.``CreatePatternAsync で新規パターンを作成できる``() =
        task {
            // Given
            let mockRepo = MockAutoJournalRepository()
            let service = AutoJournalService(mockRepo)

            // When
            let! pattern = service.CreatePatternAsync(
                "NEW001",
                "新規パターン",
                "ソーステーブル",
                Some "説明文")

            // Then
            pattern.Id |> should equal 1L
            pattern.PatternCode |> should equal "NEW001"
            pattern.PatternName |> should equal "新規パターン"
            pattern.Description |> should equal (Some "説明文")
            pattern.IsActive |> should equal true
        }

    [<Fact>]
    member this.``ActivatePatternAsync でパターンを有効化できる``() =
        task {
            // Given
            let mockRepo = MockAutoJournalRepository()
            mockRepo.SetupPatterns([createTestPattern 1L "SALES001" "売上計上" false])
            let service = AutoJournalService(mockRepo)

            // When
            let! result = service.ActivatePatternAsync("SALES001")

            // Then
            match result with
            | Ok pattern -> pattern.IsActive |> should equal true
            | Error _ -> failwith "有効化できるはず"
        }

    [<Fact>]
    member this.``DeactivatePatternAsync でパターンを無効化できる``() =
        task {
            // Given
            let mockRepo = MockAutoJournalRepository()
            mockRepo.SetupPatterns([createTestPattern 1L "SALES001" "売上計上" true])
            let service = AutoJournalService(mockRepo)

            // When
            let! result = service.DeactivatePatternAsync("SALES001")

            // Then
            match result with
            | Ok pattern -> pattern.IsActive |> should equal false
            | Error _ -> failwith "無効化できるはず"
        }

    [<Fact>]
    member this.``SetPatternItemsAsync でパターン明細を設定できる``() =
        task {
            // Given
            let mockRepo = MockAutoJournalRepository()
            mockRepo.SetupPatterns([createTestPattern 1L "SALES001" "売上計上" true])
            let service = AutoJournalService(mockRepo)
            let items = [
                createTestItem 1L 1 Debit "130"
                createTestItem 1L 2 Credit "500"
            ]

            // When
            let! result = service.SetPatternItemsAsync("SALES001", items)

            // Then
            match result with
            | Ok () -> ()
            | Error _ -> failwith "明細を設定できるはず"
        }

    [<Fact>]
    member this.``ValidatePatternForExecutionAsync で有効なパターンを検証できる``() =
        task {
            // Given
            let mockRepo = MockAutoJournalRepository()
            mockRepo.SetupPatterns([createTestPattern 1L "SALES001" "売上計上" true])
            mockRepo.SetupPatternItems(1L, [
                createTestItem 1L 1 Debit "130"
                createTestItem 1L 2 Credit "500"
            ])
            let service = AutoJournalService(mockRepo)

            // When
            let! result = service.ValidatePatternForExecutionAsync("SALES001")

            // Then
            match result with
            | Ok (pattern, items) ->
                pattern.PatternCode |> should equal "SALES001"
                items.Length |> should equal 2
            | Error _ -> failwith "検証に成功するはず"
        }

    [<Fact>]
    member this.``ValidatePatternForExecutionAsync で無効なパターンはエラーを返す``() =
        task {
            // Given
            let mockRepo = MockAutoJournalRepository()
            mockRepo.SetupPatterns([createTestPattern 1L "SALES001" "売上計上" false])
            let service = AutoJournalService(mockRepo)

            // When
            let! result = service.ValidatePatternForExecutionAsync("SALES001")

            // Then
            match result with
            | Ok _ -> failwith "エラーが返るはず"
            | Error e ->
                match e with
                | PatternInactive code -> code |> should equal "SALES001"
                | _ -> failwith "PatternInactive エラーのはず"
        }

    [<Fact>]
    member this.``ValidatePatternForExecutionAsync で明細がないパターンはエラーを返す``() =
        task {
            // Given
            let mockRepo = MockAutoJournalRepository()
            mockRepo.SetupPatterns([createTestPattern 1L "SALES001" "売上計上" true])
            // 明細を設定しない
            let service = AutoJournalService(mockRepo)

            // When
            let! result = service.ValidatePatternForExecutionAsync("SALES001")

            // Then
            match result with
            | Ok _ -> failwith "エラーが返るはず"
            | Error e ->
                match e with
                | NoPatternItems patternId -> patternId |> should equal 1L
                | _ -> failwith "NoPatternItems エラーのはず"
        }

    [<Fact>]
    member this.``LogExecutionAsync で実行ログを記録できる``() =
        task {
            // Given
            let mockRepo = MockAutoJournalRepository()
            let service = AutoJournalService(mockRepo)
            let result: AutoJournalExecutionResult = {
                PatternId = 1L
                ProcessedCount = 100
                GeneratedCount = 100
                Status = Success
                Message = Some "正常終了"
                ErrorDetail = None
            }

            // When
            let! log = service.LogExecutionAsync(result)

            // Then
            log.Id |> should equal 1L
            log.PatternId |> should equal 1L
            log.ProcessedCount |> should equal 100
            log.GeneratedCount |> should equal 100
            log.Status |> should equal Success
        }

    [<Fact>]
    member this.``GetExecutionHistoryAsync で実行履歴を取得できる``() =
        task {
            // Given
            let mockRepo = MockAutoJournalRepository()
            mockRepo.SetupPatterns([createTestPattern 1L "SALES001" "売上計上" true])
            let service = AutoJournalService(mockRepo)

            // ログを3件記録
            let! _ = service.LogExecutionAsync({
                PatternId = 1L; ProcessedCount = 10; GeneratedCount = 10
                Status = Success; Message = None; ErrorDetail = None
            })
            let! _ = service.LogExecutionAsync({
                PatternId = 1L; ProcessedCount = 20; GeneratedCount = 20
                Status = Success; Message = None; ErrorDetail = None
            })
            let! _ = service.LogExecutionAsync({
                PatternId = 1L; ProcessedCount = 30; GeneratedCount = 30
                Status = Success; Message = None; ErrorDetail = None
            })

            // When
            let! result = service.GetExecutionHistoryAsync("SALES001", 2)

            // Then
            match result with
            | Ok logs -> logs.Length |> should equal 2
            | Error _ -> failwith "履歴が取得できるはず"
        }
