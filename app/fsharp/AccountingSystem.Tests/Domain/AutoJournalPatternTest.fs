module AccountingSystem.Tests.Domain.AutoJournalPatternTest

open System
open Xunit
open AccountingSystem.Domain.Models.AutoJournalPattern
open AccountingSystem.Domain.Types

module AutoJournalPatternTest =

    [<Fact>]
    let ``AutoJournalPattern.create で自動仕訳パターンを作成できる`` () =
        let pattern = AutoJournalPattern.create "SALES001" "売上計上パターン" "売上明細"

        Assert.Equal("SALES001", pattern.PatternCode)
        Assert.Equal("売上計上パターン", pattern.PatternName)
        Assert.Equal("売上明細", pattern.SourceTableName)
        Assert.True(pattern.IsActive)
        Assert.Equal(None, pattern.Description)

    [<Fact>]
    let ``同じ PatternCode を持つ AutoJournalPattern は同一エンティティである`` () =
        let pattern1 = AutoJournalPattern.create "SALES001" "売上計上パターン" "売上明細"
        let pattern2 = { AutoJournalPattern.create "SALES001" "別名" "別テーブル" with
                            Description = Some "説明" }

        Assert.True(AutoJournalPattern.equal pattern1 pattern2)

    [<Fact>]
    let ``異なる PatternCode を持つ AutoJournalPattern は異なるエンティティである`` () =
        let pattern1 = AutoJournalPattern.create "SALES001" "売上計上パターン" "売上明細"
        let pattern2 = AutoJournalPattern.create "SALES002" "売上計上パターン" "売上明細"

        Assert.False(AutoJournalPattern.equal pattern1 pattern2)

    [<Fact>]
    let ``同じ PatternCode の AutoJournalPattern は同じハッシュコードを持つ`` () =
        let pattern1 = AutoJournalPattern.create "SALES001" "売上計上パターン" "売上明細"
        let pattern2 = { AutoJournalPattern.create "SALES001" "別名" "別テーブル" with
                            Description = Some "説明" }

        Assert.Equal(AutoJournalPattern.hashCode pattern1, AutoJournalPattern.hashCode pattern2)

    [<Fact>]
    let ``AutoJournalPatternItem.createDebit で借方明細を作成できる`` () =
        let item = AutoJournalPatternItem.createDebit 1L 1 "130" "{売上金額}"

        Assert.Equal(1L, item.PatternId)
        Assert.Equal(1, item.LineNumber)
        Assert.Equal(Debit, item.DebitCreditType)
        Assert.Equal("{売上金額}", item.AmountExpression)

    [<Fact>]
    let ``AutoJournalPatternItem.createCredit で貸方明細を作成できる`` () =
        let item = AutoJournalPatternItem.createCredit 1L 1 "500" "{売上金額}"

        Assert.Equal(1L, item.PatternId)
        Assert.Equal(1, item.LineNumber)
        Assert.Equal(Credit, item.DebitCreditType)
        Assert.Equal("{売上金額}", item.AmountExpression)

    [<Fact>]
    let ``AutoJournalLog.createSuccess で成功ログを作成できる`` () =
        let log = AutoJournalLog.createSuccess 1L 100 100

        Assert.Equal(1L, log.PatternId)
        Assert.Equal(100, log.ProcessedCount)
        Assert.Equal(100, log.GeneratedCount)
        Assert.Equal(Success, log.Status)
        Assert.True(log.Message.IsSome)
        Assert.Equal(None, log.ErrorDetail)

    [<Fact>]
    let ``AutoJournalLog.createFailure で失敗ログを作成できる`` () =
        let log = AutoJournalLog.createFailure 1L 50 "処理エラー" "詳細なエラー内容"

        Assert.Equal(1L, log.PatternId)
        Assert.Equal(50, log.ProcessedCount)
        Assert.Equal(0, log.GeneratedCount)
        Assert.Equal(Failure, log.Status)
        Assert.Equal(Some "処理エラー", log.Message)
        Assert.Equal(Some "詳細なエラー内容", log.ErrorDetail)

    [<Fact>]
    let ``AutoJournalStatus.toString と fromString で変換できる`` () =
        Assert.Equal("SUCCESS", AutoJournalStatus.toString Success)
        Assert.Equal("FAILURE", AutoJournalStatus.toString Failure)
        Assert.Equal("PARTIAL", AutoJournalStatus.toString Partial)

        Assert.Equal(Some Success, AutoJournalStatus.fromString "SUCCESS")
        Assert.Equal(Some Failure, AutoJournalStatus.fromString "FAILURE")
        Assert.Equal(Some Partial, AutoJournalStatus.fromString "PARTIAL")
        Assert.Equal(None, AutoJournalStatus.fromString "INVALID")
