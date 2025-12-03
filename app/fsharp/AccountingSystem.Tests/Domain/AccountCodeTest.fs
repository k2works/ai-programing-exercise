module AccountingSystem.Tests.Domain.AccountCodeTest

open AccountingSystem.Domain.Types
open Xunit
open FsUnit.Xunit
open System

/// <summary>
/// AccountCode 値オブジェクトのテスト
/// </summary>
type AccountCodeTest() =

    [<Fact>]
    member _.``AccountCode.Create で勘定科目コードを作成できる``() =
        let code = AccountCode.Create("1000")
        code.Code |> should equal "1000"

    [<Fact>]
    member _.``AccountCode.Create は空文字で例外を投げる``() =
        (fun () -> AccountCode.Create("") |> ignore)
        |> should throw typeof<ArgumentException>

    [<Fact>]
    member _.``AccountCode.Create は空白文字のみで例外を投げる``() =
        (fun () -> AccountCode.Create("   ") |> ignore)
        |> should throw typeof<ArgumentException>

    [<Fact>]
    member _.``AccountCode.Create は null で例外を投げる``() =
        (fun () -> AccountCode.Create(null) |> ignore)
        |> should throw typeof<ArgumentException>

    [<Fact>]
    member _.``AccountCode.Create は前後の空白をトリムする``() =
        let code = AccountCode.Create("  1000  ")
        code.Code |> should equal "1000"

    [<Fact>]
    member _.``AccountCode.CreateUnsafe はバリデーションなしで作成する``() =
        let code = AccountCode.CreateUnsafe("1000")
        code.Code |> should equal "1000"

    [<Fact>]
    member _.``AccountCode.StartsWith で前方一致を判定できる``() =
        let code = AccountCode.Create("1000")
        code.StartsWith("10") |> should equal true
        code.StartsWith("20") |> should equal false

    [<Fact>]
    member _.``AccountCode.Contains で部分一致を判定できる``() =
        let code = AccountCode.Create("1000")
        code.Contains("00") |> should equal true
        code.Contains("99") |> should equal false

    [<Fact>]
    member _.``AccountCode.ToString は文字列表現を返す``() =
        let code = AccountCode.Create("1000")
        code.ToString() |> should equal "1000"

    [<Fact>]
    member _.``同じ値の AccountCode は等しい``() =
        let a = AccountCode.Create("1000")
        let b = AccountCode.Create("1000")
        AccountCode.equal a b |> should equal true

    [<Fact>]
    member _.``異なる値の AccountCode は等しくない``() =
        let a = AccountCode.Create("1000")
        let b = AccountCode.Create("2000")
        AccountCode.equal a b |> should equal false

    [<Fact>]
    member _.``AccountCode.fromString と toString で変換できる``() =
        let original = "1000"
        let code = AccountCode.fromString original
        let result = AccountCode.toString code
        result |> should equal original
