module AccountingSystem.Tests.Domain.AccountStructureTest

open AccountingSystem.Domain.Models
open AccountingSystem.Domain.Types
open Xunit
open FsUnit.Xunit

/// <summary>
/// AccountStructure エンティティのテスト
/// </summary>
type AccountStructureTest() =

    [<Fact>]
    member _.``同じ AccountCode を持つ AccountStructure は同一エンティティである``() =
        let structure1 = AccountStructure.create "1000" "~1000~" 1 1
        let structure2 = { structure1 with AccountPath = "~資産~1000~"; HierarchyLevel = 2 }

        AccountStructure.equal structure1 structure2 |> should equal true

    [<Fact>]
    member _.``異なる AccountCode を持つ AccountStructure は異なるエンティティである``() =
        let structure1 = AccountStructure.create "1000" "~1000~" 1 1
        let structure2 = AccountStructure.create "1100" "~1100~" 1 2

        AccountStructure.equal structure1 structure2 |> should equal false

    [<Fact>]
    member _.``同じ AccountCode の AccountStructure は同じハッシュコードを持つ``() =
        let structure1 = AccountStructure.create "1000" "~1000~" 1 1
        let structure2 = { structure1 with AccountPath = "~資産~1000~"; DisplayOrder = 99 }

        AccountStructure.hashCode structure1 |> should equal (AccountStructure.hashCode structure2)

    [<Fact>]
    member _.``属性が変更されても AccountCode が同じなら同一エンティティである``() =
        let original = AccountStructure.create "1000" "~1000~" 1 1
        let modified = { original with
                            AccountPath = "~資産~流動資産~1000~"
                            HierarchyLevel = 3
                            ParentAccountCode = Some (AccountCode.Create("流動資産"))
                            DisplayOrder = 100 }

        AccountStructure.equal original modified |> should equal true
