module AccountingSystem.Tests.Domain.AccountTest

open AccountingSystem.Domain.Models
open AccountingSystem.Domain.Types
open Xunit
open FsUnit.Xunit

/// <summary>
/// Account エンティティのテスト
/// </summary>
type AccountTest() =

    [<Fact>]
    member _.``同じ AccountCode を持つ Account は同一エンティティである``() =
        let account1 = Account.create "1000" "現金" AccountType.Asset false
        let account2 = { account1 with AccountName = "現金及び預金"; Balance = Money.Create(50000m) }

        Account.equal account1 account2 |> should equal true

    [<Fact>]
    member _.``異なる AccountCode を持つ Account は異なるエンティティである``() =
        let account1 = Account.create "1000" "現金" AccountType.Asset false
        let account2 = Account.create "1100" "現金" AccountType.Asset false

        Account.equal account1 account2 |> should equal false

    [<Fact>]
    member _.``同じ AccountCode の Account は同じハッシュコードを持つ``() =
        let account1 = Account.create "1000" "現金" AccountType.Asset false
        let account2 = { account1 with AccountName = "現金及び預金"; Balance = Money.Create(50000m) }

        Account.hashCode account1 |> should equal (Account.hashCode account2)

    [<Fact>]
    member _.``属性が変更されても AccountCode が同じなら同一エンティティである``() =
        let original = Account.create "1000" "現金" AccountType.Asset false
        let modified = { original with
                            AccountName = "変更後の名前"
                            AccountNameKana = Some "ヘンコウゴノナマエ"
                            Balance = Money.Create(999999m)
                            BsplType = Some BsplType.BalanceSheet
                            TransactionElementType = Some TransactionElementType.AssetElement }

        Account.equal original modified |> should equal true
