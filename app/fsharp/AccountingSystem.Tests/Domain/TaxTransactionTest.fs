module AccountingSystem.Tests.Domain.TaxTransactionTest

open AccountingSystem.Domain.Models
open Xunit
open FsUnit.Xunit

/// <summary>
/// TaxTransaction エンティティのテスト
/// </summary>
type TaxTransactionTest() =

    [<Fact>]
    member _.``同じ TaxCode を持つ TaxTransaction は同一エンティティである``() =
        let tax1 = TaxTransaction.create "T10" "課税売上10%" 0.10m
        let tax2 = { tax1 with TaxName = "消費税10%"; TaxRate = 0.10m }

        TaxTransaction.equal tax1 tax2 |> should equal true

    [<Fact>]
    member _.``異なる TaxCode を持つ TaxTransaction は異なるエンティティである``() =
        let tax1 = TaxTransaction.create "T10" "課税売上10%" 0.10m
        let tax2 = TaxTransaction.create "T08" "課税売上8%" 0.08m

        TaxTransaction.equal tax1 tax2 |> should equal false

    [<Fact>]
    member _.``同じ TaxCode の TaxTransaction は同じハッシュコードを持つ``() =
        let tax1 = TaxTransaction.create "T10" "課税売上10%" 0.10m
        let tax2 = { tax1 with TaxName = "消費税10%"; Description = Some "標準税率" }

        TaxTransaction.hashCode tax1 |> should equal (TaxTransaction.hashCode tax2)

    [<Fact>]
    member _.``属性が変更されても TaxCode が同じなら同一エンティティである``() =
        let original = TaxTransaction.create "T10" "課税売上10%" 0.10m
        let modified = { original with
                            TaxName = "変更後の名前"
                            TaxRate = 0.15m
                            Description = Some "説明文" }

        TaxTransaction.equal original modified |> should equal true
