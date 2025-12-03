module AccountingSystem.Tests.Domain.JournalLineTest

open System
open AccountingSystem.Domain.Models.JournalLine
open Xunit
open FsUnit.Xunit

/// <summary>
/// JournalLine エンティティのテスト（3層構造2層目）
/// </summary>
type JournalLineTest() =

    [<Fact>]
    member _.``JournalLine.create で仕訳明細を作成できる``() =
        let line = JournalLine.create "JN240001" 1 "現金売上（消費税込）"

        line.VoucherNumber.Number |> should equal "JN240001"
        line.LineNumber |> should equal 1
        line.Description |> should equal "現金売上（消費税込）"

    [<Fact>]
    member _.``同じ VoucherNumber と LineNumber を持つ JournalLine は同一エンティティである``() =
        let line1 = JournalLine.create "JN240001" 1 "現金売上"
        let line2 = { line1 with Description = "変更後" }

        JournalLine.equal line1 line2 |> should equal true

    [<Fact>]
    member _.``異なる LineNumber を持つ JournalLine は異なるエンティティである``() =
        let line1 = JournalLine.create "JN240001" 1 "現金売上"
        let line2 = JournalLine.create "JN240001" 2 "現金売上"

        JournalLine.equal line1 line2 |> should equal false

    [<Fact>]
    member _.``異なる VoucherNumber を持つ JournalLine は異なるエンティティである``() =
        let line1 = JournalLine.create "JN240001" 1 "現金売上"
        let line2 = JournalLine.create "JN240002" 1 "現金売上"

        JournalLine.equal line1 line2 |> should equal false

    [<Fact>]
    member _.``同じ VoucherNumber と LineNumber の JournalLine は同じハッシュコードを持つ``() =
        let line1 = JournalLine.create "JN240001" 1 "現金売上"
        let line2 = { line1 with Description = "変更後" }

        JournalLine.hashCode line1 |> should equal (JournalLine.hashCode line2)
