module AccountingSystem.Tests.Domain.JournalTest

open System
open AccountingSystem.Domain.Models.Journal
open AccountingSystem.Domain.Types
open Xunit
open FsUnit.Xunit

/// <summary>
/// Journal エンティティのテスト（3層構造ヘッダー）
/// </summary>
type JournalTest() =

    [<Fact>]
    member _.``Journal.create で仕訳を作成できる``() =
        let journal = Journal.create "JN240001" (DateTime(2024, 1, 15)) (DateTime(2024, 1, 15)) Transfer

        journal.VoucherNumber.Number |> should equal "JN240001"
        journal.PostingDate |> should equal (DateTime(2024, 1, 15))
        journal.EntryDate |> should equal (DateTime(2024, 1, 15))
        journal.VoucherType |> should equal Transfer
        journal.SettlementFlag |> should equal SettlementFlag.Normal
        journal.RedSlipFlag |> should equal RedSlipFlag.Normal
        journal.RedBlackVoucherNumber |> should equal None

    [<Fact>]
    member _.``Journal.createSettlement で決算仕訳を作成できる``() =
        let journal = Journal.createSettlement "JN240002" (DateTime(2024, 3, 31)) (DateTime(2024, 4, 1))

        journal.VoucherNumber.Number |> should equal "JN240002"
        journal.SettlementFlag |> should equal SettlementFlag.Settlement
        Journal.isSettlement journal |> should equal true

    [<Fact>]
    member _.``Journal.createRedSlip で赤伝票を作成できる``() =
        let journal = Journal.createRedSlip "JN240003" (DateTime(2024, 1, 20)) (DateTime(2024, 1, 20)) 1

        journal.VoucherNumber.Number |> should equal "JN240003"
        journal.RedSlipFlag |> should equal RedSlipFlag.RedSlip
        journal.RedBlackVoucherNumber |> should equal (Some 1)
        Journal.isRedSlip journal |> should equal true

    [<Fact>]
    member _.``同じ VoucherNumber を持つ Journal は同一エンティティである``() =
        let journal1 = Journal.create "JN240001" (DateTime(2024, 1, 15)) (DateTime(2024, 1, 15)) Transfer
        let journal2 = { journal1 with PostingDate = DateTime(2024, 2, 1) }

        Journal.equal journal1 journal2 |> should equal true

    [<Fact>]
    member _.``異なる VoucherNumber を持つ Journal は異なるエンティティである``() =
        let journal1 = Journal.create "JN240001" (DateTime(2024, 1, 15)) (DateTime(2024, 1, 15)) Transfer
        let journal2 = Journal.create "JN240002" (DateTime(2024, 1, 15)) (DateTime(2024, 1, 15)) Transfer

        Journal.equal journal1 journal2 |> should equal false

    [<Fact>]
    member _.``同じ VoucherNumber の Journal は同じハッシュコードを持つ``() =
        let journal1 = Journal.create "JN240001" (DateTime(2024, 1, 15)) (DateTime(2024, 1, 15)) Transfer
        let journal2 = { journal1 with PostingDate = DateTime(2024, 2, 1) }

        Journal.hashCode journal1 |> should equal (Journal.hashCode journal2)
