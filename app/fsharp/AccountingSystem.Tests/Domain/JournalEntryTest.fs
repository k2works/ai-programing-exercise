module AccountingSystem.Tests.Domain.JournalEntryTest

open System
open AccountingSystem.Domain.Models
open AccountingSystem.Domain.Types
open Xunit
open FsUnit.Xunit

/// <summary>
/// JournalEntry エンティティのテスト
/// </summary>
type JournalEntryTest() =

    [<Fact>]
    member _.``JournalEntry.create で仕訳エントリを作成できる``() =
        let entry = JournalEntry.create "JE240001" (DateTime(2024, 1, 15)) "現金売上" (Money.Create(110000m)) "user001"

        entry.VoucherNumber.Number |> should equal "JE240001"
        entry.EntryDate |> should equal (DateTime(2024, 1, 15))
        entry.Description |> should equal "現金売上"
        entry.TotalAmount.Amount |> should equal 110000m
        entry.CreatedBy |> should equal "user001"
        entry.ReferenceNumber |> should equal None

    [<Fact>]
    member _.``同じ VoucherNumber を持つ JournalEntry は同一エンティティである``() =
        let entry1 = JournalEntry.create "JE240001" (DateTime(2024, 1, 15)) "現金売上" (Money.Create(110000m)) "user001"
        let entry2 = { entry1 with Description = "変更後"; TotalAmount = Money.Create(200000m) }

        JournalEntry.equal entry1 entry2 |> should equal true

    [<Fact>]
    member _.``異なる VoucherNumber を持つ JournalEntry は異なるエンティティである``() =
        let entry1 = JournalEntry.create "JE240001" (DateTime(2024, 1, 15)) "現金売上" (Money.Create(110000m)) "user001"
        let entry2 = JournalEntry.create "JE240002" (DateTime(2024, 1, 15)) "現金売上" (Money.Create(110000m)) "user001"

        JournalEntry.equal entry1 entry2 |> should equal false

    [<Fact>]
    member _.``同じ VoucherNumber の JournalEntry は同じハッシュコードを持つ``() =
        let entry1 = JournalEntry.create "JE240001" (DateTime(2024, 1, 15)) "現金売上" (Money.Create(110000m)) "user001"
        let entry2 = { entry1 with Description = "変更後" }

        JournalEntry.hashCode entry1 |> should equal (JournalEntry.hashCode entry2)
