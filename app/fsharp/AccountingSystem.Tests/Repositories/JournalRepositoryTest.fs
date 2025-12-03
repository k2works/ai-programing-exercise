module AccountingSystem.Tests.Repositories.JournalRepositoryTest

open System
open AccountingSystem.Domain.Models.Journal
open AccountingSystem.Domain.Models.JournalLine
open AccountingSystem.Domain.Models.JournalLineItem
open AccountingSystem.Domain.Types
open AccountingSystem.Domain.Types.Measure
open AccountingSystem.Infrastructure.Repositories.JournalRepository
open AccountingSystem.Tests.DatabaseTestBase
open Npgsql
open Xunit
open FsUnit.Xunit

/// <summary>
/// 仕訳リポジトリ（集約ルート）の統合テスト
/// 3層構造（Journal → JournalLine → JournalLineItem）の永続化をテスト
/// 金額は日本円（円）単位で管理
/// </summary>
type JournalRepositoryTest() =
    inherit DatabaseTestBase()

    /// テスト用の勘定科目をセットアップ
    member private this.SetupTestAccountsAsync() =
        task {
            use conn = new NpgsqlConnection(this.ConnectionString)
            do! conn.OpenAsync()

            // テスト用勘定科目を登録（存在しない場合のみ）
            use cmd = new NpgsqlCommand("""
                INSERT INTO "勘定科目マスタ" ("勘定科目コード", "勘定科目名", "勘定科目種別", "残高")
                VALUES ('11100', '現金', '資産', 0)
                ON CONFLICT ("勘定科目コード") DO NOTHING;

                INSERT INTO "勘定科目マスタ" ("勘定科目コード", "勘定科目名", "勘定科目種別", "残高")
                VALUES ('21100', '買掛金', '負債', 0)
                ON CONFLICT ("勘定科目コード") DO NOTHING;

                INSERT INTO "勘定科目マスタ" ("勘定科目コード", "勘定科目名", "勘定科目種別", "残高")
                VALUES ('41100', '売上高', '収益', 0)
                ON CONFLICT ("勘定科目コード") DO NOTHING;
            """, conn)
            let! _ = cmd.ExecuteNonQueryAsync()
            ()
        }

    /// テストデータをクリーンアップ
    member private this.CleanupTestDataAsync() =
        task {
            use conn = new NpgsqlConnection(this.ConnectionString)
            do! conn.OpenAsync()

            use cmd = new NpgsqlCommand("""
                DELETE FROM "仕訳貸借明細" WHERE "仕訳伝票番号" LIKE 'TEST%';
                DELETE FROM "仕訳明細" WHERE "仕訳伝票番号" LIKE 'TEST%';
                DELETE FROM "仕訳" WHERE "仕訳伝票番号" LIKE 'TEST%';
            """, conn)
            let! _ = cmd.ExecuteNonQueryAsync()
            ()
        }

    /// テスト用の仕訳貸借明細を作成（金額は円単位）
    member private _.CreateTestLineItem(voucherNumber: string, lineNumber: int, debitCreditType: DebitCreditType, accountCode: string, amount: decimal<円>) =
        {
            VoucherNumber = VoucherNumber.Create(voucherNumber)
            LineNumber = lineNumber
            DebitCreditType = debitCreditType
            ExchangeRate = 1.0m
            DepartmentCode = None
            ProjectCode = None
            AccountCode = AccountCode.Create(accountCode)
            SubAccountCode = None
            Amount = CurrencyAmount.CreateJPY円(amount)
            BaseAmount = Money.Create円(amount)
            TaxCategory = None
            TaxRate = None
            TaxCalculationType = None
            DueDate = None
            IsCashFlow = false
            SegmentCode = None
            CounterAccountCode = None
            CounterSubAccountCode = None
            MemoCode = None
            MemoContent = None
        }

    /// テスト用の仕訳明細を作成
    member private this.CreateTestLine(voucherNumber: string, lineNumber: int, description: string, items: JournalLineItem list) =
        {
            VoucherNumber = VoucherNumber.Create(voucherNumber)
            LineNumber = lineNumber
            Description = description
            Items = items
        }

    /// テスト用の仕訳を作成
    member private _.CreateTestJournal(voucherNumber: string, lines: JournalLine list) =
        {
            VoucherNumber = VoucherNumber.Create(voucherNumber)
            PostingDate = DateTime(2024, 1, 15)
            EntryDate = DateTime(2024, 1, 15)
            SettlementFlag = SettlementFlag.Normal
            IsSingleEntry = false
            VoucherType = Transfer
            IsRecurring = false
            EmployeeCode = Some "EMP001"
            DepartmentCode = Some "D01"
            RedSlipFlag = RedSlipFlag.Normal
            RedBlackVoucherNumber = None
            Lines = lines
        }

    [<Fact>]
    member this.``仕訳集約を登録できる``() =
        task {
            do! this.SetupTestAccountsAsync()
            do! this.CleanupTestDataAsync()

            // 貸借明細（借方：現金 10,000円、貸方：売上 10,000円）
            let debitItem = this.CreateTestLineItem("TEST0001", 1, Debit, "11100", 10000m<円>)
            let creditItem = this.CreateTestLineItem("TEST0001", 1, Credit, "41100", 10000m<円>)

            // 明細
            let line = this.CreateTestLine("TEST0001", 1, "テスト取引", [debitItem; creditItem])

            // 仕訳
            let journal = this.CreateTestJournal("TEST0001", [line])

            // 登録
            let! voucherNumber = insertAsync this.ConnectionString journal
            voucherNumber |> should equal "TEST0001"

            // クリーンアップ
            do! this.CleanupTestDataAsync()
        }

    [<Fact>]
    member this.``仕訳集約を伝票番号で検索できる``() =
        task {
            do! this.SetupTestAccountsAsync()
            do! this.CleanupTestDataAsync()

            // テストデータ作成（借方：現金 50,000円、貸方：買掛金 50,000円）
            let debitItem = this.CreateTestLineItem("TEST0002", 1, Debit, "11100", 50000m<円>)
            let creditItem = this.CreateTestLineItem("TEST0002", 1, Credit, "21100", 50000m<円>)
            let line = this.CreateTestLine("TEST0002", 1, "検索テスト取引", [debitItem; creditItem])
            let journal = this.CreateTestJournal("TEST0002", [line])

            // 登録
            let! _ = insertAsync this.ConnectionString journal

            // 検索
            let! found = findByVoucherNumberAsync this.ConnectionString "TEST0002"

            found.IsSome |> should equal true
            found.Value.VoucherNumber.Number |> should equal "TEST0002"
            found.Value.Lines |> List.length |> should equal 1
            found.Value.Lines.[0].Items |> List.length |> should equal 2
            found.Value.Lines.[0].Description |> should equal "検索テスト取引"

            // クリーンアップ
            do! this.CleanupTestDataAsync()
        }

    [<Fact>]
    member this.``存在しない伝票番号で検索するとNoneを返す``() =
        task {
            let! found = findByVoucherNumberAsync this.ConnectionString "NOTEXIST"
            found.IsNone |> should equal true
        }

    [<Fact>]
    member this.``複数明細を持つ仕訳を登録・検索できる``() =
        task {
            do! this.SetupTestAccountsAsync()
            do! this.CleanupTestDataAsync()

            // 明細1：現金 10,000円 / 売上 10,000円
            let debitItem1 = this.CreateTestLineItem("TEST0003", 1, Debit, "11100", 10000m<円>)
            let creditItem1 = this.CreateTestLineItem("TEST0003", 1, Credit, "41100", 10000m<円>)
            let line1 = this.CreateTestLine("TEST0003", 1, "売上取引1", [debitItem1; creditItem1])

            // 明細2：現金 20,000円 / 売上 20,000円
            let debitItem2 = this.CreateTestLineItem("TEST0003", 2, Debit, "11100", 20000m<円>)
            let creditItem2 = this.CreateTestLineItem("TEST0003", 2, Credit, "41100", 20000m<円>)
            let line2 = this.CreateTestLine("TEST0003", 2, "売上取引2", [debitItem2; creditItem2])

            let journal = this.CreateTestJournal("TEST0003", [line1; line2])

            // 登録
            let! _ = insertAsync this.ConnectionString journal

            // 検索
            let! found = findByVoucherNumberAsync this.ConnectionString "TEST0003"

            found.IsSome |> should equal true
            found.Value.Lines |> List.length |> should equal 2
            found.Value.Lines.[0].LineNumber |> should equal 1
            found.Value.Lines.[1].LineNumber |> should equal 2

            // クリーンアップ
            do! this.CleanupTestDataAsync()
        }

    [<Fact>]
    member this.``起票日範囲で仕訳を検索できる``() =
        task {
            do! this.SetupTestAccountsAsync()
            do! this.CleanupTestDataAsync()

            // テストデータ（異なる起票日）1,000円と2,000円
            let item1 = this.CreateTestLineItem("TEST0004", 1, Debit, "11100", 1000m<円>)
            let item2 = this.CreateTestLineItem("TEST0004", 1, Credit, "41100", 1000m<円>)
            let line = this.CreateTestLine("TEST0004", 1, "日付テスト", [item1; item2])
            let journal1 = { this.CreateTestJournal("TEST0004", [line]) with PostingDate = DateTime(2024, 1, 15) }

            let item3 = this.CreateTestLineItem("TEST0005", 1, Debit, "11100", 2000m<円>)
            let item4 = this.CreateTestLineItem("TEST0005", 1, Credit, "41100", 2000m<円>)
            let line2 = this.CreateTestLine("TEST0005", 1, "日付テスト2", [item3; item4])
            let journal2 = { this.CreateTestJournal("TEST0005", [line2]) with PostingDate = DateTime(2024, 2, 20) }

            let! _ = insertAsync this.ConnectionString journal1
            let! _ = insertAsync this.ConnectionString journal2

            // 1月の仕訳を検索
            let! januaryJournals = findByDateRangeAsync this.ConnectionString (DateTime(2024, 1, 1)) (DateTime(2024, 1, 31))
            januaryJournals |> List.exists (fun j -> j.VoucherNumber.Number = "TEST0004") |> should equal true
            januaryJournals |> List.exists (fun j -> j.VoucherNumber.Number = "TEST0005") |> should equal false

            // クリーンアップ
            do! this.CleanupTestDataAsync()
        }

    [<Fact>]
    member this.``仕訳ヘッダーを更新できる``() =
        task {
            do! this.SetupTestAccountsAsync()
            do! this.CleanupTestDataAsync()

            // 5,000円の仕訳
            let item1 = this.CreateTestLineItem("TEST0006", 1, Debit, "11100", 5000m<円>)
            let item2 = this.CreateTestLineItem("TEST0006", 1, Credit, "41100", 5000m<円>)
            let line = this.CreateTestLine("TEST0006", 1, "更新テスト", [item1; item2])
            let journal = this.CreateTestJournal("TEST0006", [line])

            let! _ = insertAsync this.ConnectionString journal

            // 更新
            let updated = { journal with
                              EmployeeCode = Some "EMP999"
                              DepartmentCode = Some "D99" }
            let! rowsAffected = updateAsync this.ConnectionString updated
            rowsAffected |> should equal 1

            // 確認
            let! found = findByVoucherNumberAsync this.ConnectionString "TEST0006"
            found.Value.EmployeeCode |> should equal (Some "EMP999")
            found.Value.DepartmentCode |> should equal (Some "D99")

            // クリーンアップ
            do! this.CleanupTestDataAsync()
        }

    [<Fact>]
    member this.``仕訳を削除すると明細も連鎖削除される``() =
        task {
            do! this.SetupTestAccountsAsync()
            do! this.CleanupTestDataAsync()

            let item1 = this.CreateTestLineItem("TEST0007", 1, Debit, "11100", 3000m<円>)
            let item2 = this.CreateTestLineItem("TEST0007", 1, Credit, "41100", 3000m<円>)
            let line = this.CreateTestLine("TEST0007", 1, "削除テスト", [item1; item2])
            let journal = this.CreateTestJournal("TEST0007", [line])

            let! _ = insertAsync this.ConnectionString journal

            // 削除
            let! rowsAffected = deleteAsync this.ConnectionString "TEST0007"
            rowsAffected |> should equal 1

            // 確認
            let! found = findByVoucherNumberAsync this.ConnectionString "TEST0007"
            found.IsNone |> should equal true
        }

    [<Fact>]
    member this.``決算仕訳を登録・検索できる``() =
        task {
            do! this.SetupTestAccountsAsync()
            do! this.CleanupTestDataAsync()

            let item1 = this.CreateTestLineItem("TEST0008", 1, Debit, "11100", 100000m<円>)
            let item2 = this.CreateTestLineItem("TEST0008", 1, Credit, "41100", 100000m<円>)
            let line = this.CreateTestLine("TEST0008", 1, "決算振替", [item1; item2])
            let journal = { this.CreateTestJournal("TEST0008", [line]) with
                              SettlementFlag = SettlementFlag.Settlement }

            let! _ = insertAsync this.ConnectionString journal

            let! found = findByVoucherNumberAsync this.ConnectionString "TEST0008"
            found.Value.SettlementFlag |> should equal SettlementFlag.Settlement
            Journal.isSettlement found.Value |> should equal true

            // クリーンアップ
            do! this.CleanupTestDataAsync()
        }

    [<Fact>]
    member this.``赤伝票を登録・検索できる``() =
        task {
            do! this.SetupTestAccountsAsync()
            do! this.CleanupTestDataAsync()

            let item1 = this.CreateTestLineItem("TEST0009", 1, Debit, "11100", 5000m<円>)
            let item2 = this.CreateTestLineItem("TEST0009", 1, Credit, "41100", 5000m<円>)
            let line = this.CreateTestLine("TEST0009", 1, "赤伝票テスト", [item1; item2])
            let journal = { this.CreateTestJournal("TEST0009", [line]) with
                              RedSlipFlag = RedSlipFlag.RedSlip
                              RedBlackVoucherNumber = Some 12345 }

            let! _ = insertAsync this.ConnectionString journal

            let! found = findByVoucherNumberAsync this.ConnectionString "TEST0009"
            found.Value.RedSlipFlag |> should equal RedSlipFlag.RedSlip
            found.Value.RedBlackVoucherNumber |> should equal (Some 12345)
            Journal.isRedSlip found.Value |> should equal true

            // クリーンアップ
            do! this.CleanupTestDataAsync()
        }

    [<Fact>]
    member this.``貸借バランスが取れている仕訳を検証できる``() =
        task {
            do! this.SetupTestAccountsAsync()
            do! this.CleanupTestDataAsync()

            let debitItem = this.CreateTestLineItem("TEST0010", 1, Debit, "11100", 25000m<円>)
            let creditItem = this.CreateTestLineItem("TEST0010", 1, Credit, "41100", 25000m<円>)
            let line = this.CreateTestLine("TEST0010", 1, "バランステスト", [debitItem; creditItem])
            let journal = this.CreateTestJournal("TEST0010", [line])

            let! _ = insertAsync this.ConnectionString journal

            let! found = findByVoucherNumberAsync this.ConnectionString "TEST0010"

            // 借方・貸方合計を確認
            Journal.sumDebit found.Value |> should equal (Money.Create(25000m))
            Journal.sumCredit found.Value |> should equal (Money.Create(25000m))
            Journal.validateBalance found.Value |> should equal true

            // クリーンアップ
            do! this.CleanupTestDataAsync()
        }
