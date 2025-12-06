module AccountingSystem.Tests.Infrastructure.TaxTransactionRepositoryTest

open AccountingSystem.Domain.Models
open AccountingSystem.Infrastructure.Persistence.Repositories.TaxTransactionRepository
open AccountingSystem.Tests.DatabaseTestBase
open Xunit
open FsUnit.Xunit

/// <summary>
/// 課税取引マスタ - Dapper 統合テスト
/// </summary>
type TaxTransactionRepositoryTest() =
    inherit DatabaseTestBase()

    [<Fact>]
    member this.``初期データが投入されている``() =
        task {
            let! all = findAllAsync this.ConnectionString
            all |> List.length |> should be (greaterThanOrEqualTo 8)

            let! taxable = findByCodeAsync this.ConnectionString "01"
            taxable.IsSome |> should equal true
            taxable.Value.TaxName |> should equal "課税売上10%"
            taxable.Value.TaxRate |> should equal 10.00m
        }

    [<Fact>]
    member this.``課税取引コードで検索できる``() =
        task {
            let! found = findByCodeAsync this.ConnectionString "02"
            found.IsSome |> should equal true
            found.Value.TaxName |> should equal "課税売上8%（軽減）"
            found.Value.TaxRate |> should equal 8.00m
        }

    [<Fact>]
    member this.``動的SQL_課税取引名で部分一致検索``() =
        task {
            // "課税"を含む取引を検索
            let! results = searchAsync this.ConnectionString (Some "課税") None
            results |> List.length |> should be (greaterThanOrEqualTo 4)
            results |> List.forall (fun t -> t.TaxName.Contains("課税")) |> should equal true
        }

    [<Fact>]
    member this.``動的SQL_最小税率で検索``() =
        task {
            // 税率が5以上の取引を検索（10%, 8%）
            let! results = searchAsync this.ConnectionString None (Some 5.00m)
            results |> List.length |> should be (greaterThanOrEqualTo 4)
            results |> List.forall (fun t -> t.TaxRate >= 5.00m) |> should equal true
        }

    [<Fact>]
    member this.``存在しない課税取引コードはNoneを返す``() =
        task {
            let! found = findByCodeAsync this.ConnectionString "99"
            found.IsNone |> should equal true
        }
