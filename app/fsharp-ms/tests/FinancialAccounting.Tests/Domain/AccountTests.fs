namespace FinancialAccounting.Tests.Domain

open Xunit
open FinancialAccounting.Domain.Entities

module AccountTests =

    [<Fact>]
    let ``新しい勘定科目を作成できる`` () =
        // Arrange & Act
        let result = Account.create "1110" "現金" Asset BalanceSheet Debit

        // Assert
        match result with
        | Ok account ->
            Assert.Equal("1110", account.AccountCode)
            Assert.Equal("現金", account.AccountName)
            Assert.Equal(Asset, account.AccountType)
            Assert.Equal(BalanceSheet, account.BsPlType)
            Assert.Equal(Debit, account.TransactionElementType)
            Assert.Equal(0m, account.Balance)
        | Error msg ->
            Assert.Fail(msg)

    [<Fact>]
    let ``勘定科目コードが空の場合はエラー`` () =
        // Arrange & Act
        let result = Account.create "" "現金" Asset BalanceSheet Debit

        // Assert
        match result with
        | Ok _ -> Assert.Fail("エラーが期待されます")
        | Error msg -> Assert.Contains("勘定科目コード", msg)

    [<Fact>]
    let ``勘定科目名が空の場合はエラー`` () =
        // Arrange & Act
        let result = Account.create "1110" "" Asset BalanceSheet Debit

        // Assert
        match result with
        | Ok _ -> Assert.Fail("エラーが期待されます")
        | Error msg -> Assert.Contains("勘定科目名", msg)

    [<Fact>]
    let ``AccountType を文字列に変換できる`` () =
        Assert.Equal("Asset", Account.accountTypeToString Asset)
        Assert.Equal("Liability", Account.accountTypeToString Liability)
        Assert.Equal("Equity", Account.accountTypeToString Equity)
        Assert.Equal("Revenue", Account.accountTypeToString Revenue)
        Assert.Equal("Expense", Account.accountTypeToString Expense)

    [<Fact>]
    let ``文字列を AccountType に変換できる`` () =
        Assert.Equal(Some Asset, Account.stringToAccountType "asset")
        Assert.Equal(Some Liability, Account.stringToAccountType "Liability")
        Assert.Equal(Some Equity, Account.stringToAccountType "EQUITY")
        Assert.Equal(None, Account.stringToAccountType "invalid")

    [<Fact>]
    let ``BsPlType を文字列に変換できる`` () =
        Assert.Equal("B", Account.bsPlTypeToString BalanceSheet)
        Assert.Equal("P", Account.bsPlTypeToString ProfitAndLoss)

    [<Fact>]
    let ``文字列を BsPlType に変換できる`` () =
        Assert.Equal(Some BalanceSheet, Account.stringToBsPlType "B")
        Assert.Equal(Some ProfitAndLoss, Account.stringToBsPlType "p")
        Assert.Equal(None, Account.stringToBsPlType "X")

    [<Fact>]
    let ``勘定科目コードのバリデーション - 正常`` () =
        let result = Account.validateAccountCode "1110"
        Assert.Equal(Ok "1110", result)

    [<Fact>]
    let ``勘定科目コードのバリデーション - 21文字以上はエラー`` () =
        let longCode = String.replicate 21 "1"
        let result = Account.validateAccountCode longCode
        match result with
        | Ok _ -> Assert.Fail("エラーが期待されます")
        | Error msg -> Assert.Contains("20文字以内", msg)
