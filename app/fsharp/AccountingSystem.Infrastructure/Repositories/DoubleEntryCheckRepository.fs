module AccountingSystem.Infrastructure.Repositories.DoubleEntryCheckRepository

open AccountingSystem.Domain.Services.DoubleEntryBookkeepingService
open Dapper
open Npgsql

/// <summary>
/// データベースから返される不整合仕訳のDAO
/// </summary>
[<CLIMutable>]
type InconsistentJournalDao = {
    不整合伝票番号: string
    差額: decimal
}

/// <summary>
/// 3層構造用のデータベースから返される仕訳残高チェック結果のDAO
/// </summary>
[<CLIMutable>]
type JournalBalanceCheckDao = {
    VoucherNumber: string
    DebitTotal: decimal
    CreditTotal: decimal
    Difference: decimal
}

/// DAO から ドメインモデルへの変換
let private toInconsistentJournal (dao: InconsistentJournalDao) : InconsistentJournal =
    {
        VoucherNumber = dao.不整合伝票番号
        DebitTotal = 0m  // 関数は差額のみ返す
        CreditTotal = 0m
        Difference = dao.差額
    }

/// <summary>
/// 複式簿記チェックを実行（仕訳貸借明細テーブル用）
/// PostgreSQL関数「複式簿記チェック」を呼び出し
/// </summary>
let checkDoubleEntryBookkeepingAsync (connectionString: string) =
    task {
        use conn = new NpgsqlConnection(connectionString)
        do! conn.OpenAsync()

        let sql = """SELECT * FROM "複式簿記チェック"()"""

        let! results = conn.QueryAsync<InconsistentJournalDao>(sql)

        let inconsistencies =
            results
            |> Seq.map toInconsistentJournal
            |> Seq.toList

        match inconsistencies with
        | [] -> return Valid
        | _ -> return Invalid inconsistencies
    }

/// <summary>
/// 仕訳残高チェックビューから全仕訳の残高状況を取得
/// </summary>
let getAllJournalBalancesAsync (connectionString: string) =
    task {
        use conn = new NpgsqlConnection(connectionString)
        do! conn.OpenAsync()

        let sql = """
            SELECT
                "仕訳伝票番号" AS "VoucherNumber",
                "借方合計" AS "DebitTotal",
                "貸方合計" AS "CreditTotal",
                "差額" AS "Difference"
            FROM "仕訳残高チェック"
            ORDER BY "仕訳伝票番号"
        """

        let! results = conn.QueryAsync<JournalBalanceCheckDao>(sql)

        return
            results
            |> Seq.map (fun dao ->
                {
                    VoucherNumber = dao.VoucherNumber
                    DebitTotal = dao.DebitTotal
                    CreditTotal = dao.CreditTotal
                    Difference = dao.Difference
                })
            |> Seq.toList
    }

/// <summary>
/// 特定の伝票番号の複式簿記チェック
/// </summary>
let checkVoucherAsync (connectionString: string) (voucherNumber: string) =
    task {
        use conn = new NpgsqlConnection(connectionString)
        do! conn.OpenAsync()

        let sql = """
            SELECT
                "仕訳伝票番号" AS "VoucherNumber",
                "借方合計" AS "DebitTotal",
                "貸方合計" AS "CreditTotal",
                "差額" AS "Difference"
            FROM "仕訳残高チェック"
            WHERE "仕訳伝票番号" = @VoucherNumber
        """

        let! result = conn.QuerySingleOrDefaultAsync<JournalBalanceCheckDao>(
            sql,
            {| VoucherNumber = voucherNumber |})

        if isNull (box result) then
            return None
        else
            return Some {
                VoucherNumber = result.VoucherNumber
                DebitTotal = result.DebitTotal
                CreditTotal = result.CreditTotal
                Difference = result.Difference
            }
    }

/// <summary>
/// 複式簿記チェックを実行し、結果を検証
/// エラーがある場合は Result.Error を返す
/// </summary>
let validateDoubleEntryAsync (connectionString: string) =
    task {
        let! result = checkDoubleEntryBookkeepingAsync connectionString

        match result with
        | Valid ->
            return Ok "すべての仕訳が複式簿記の原理を満たしています"
        | Invalid journals ->
            let messages =
                journals
                |> List.map (fun j ->
                    sprintf "伝票番号: %s, 差額: %M" j.VoucherNumber j.Difference)
            return Error (String.concat "\n" messages)
    }
