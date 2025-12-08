namespace FinancialAccounting.Infrastructure.Persistence.Repositories

open System
open System.Data
open System.Threading.Tasks
open Dapper
open Npgsql
open FinancialAccounting.Domain.Entities
open FinancialAccounting.Application.Ports.Out

/// <summary>
/// DB 行モデル（journals テーブル）
/// </summary>
[<CLIMutable>]
type JournalRow = {
    journal_id: int
    journal_date: DateTime
    description: string
    fiscal_year: int
    created_at: DateTime
    updated_at: DateTime
}

/// <summary>
/// DB 行モデル（journal_entries テーブル）
/// </summary>
[<CLIMutable>]
type JournalEntryRow = {
    entry_id: int
    journal_id: int
    account_code: string
    debit_amount: decimal
    credit_amount: decimal
    description: string
}

/// <summary>
/// Dapper を使用した仕訳リポジトリの実装
/// </summary>
type JournalRepository(connectionString: string) =

    let createConnection () : IDbConnection =
        new NpgsqlConnection(connectionString)

    let toJournalEntry (row: JournalEntryRow) : JournalEntry =
        {
            JournalId = Some row.journal_id
            AccountCode = row.account_code
            DebitAmount = row.debit_amount
            CreditAmount = row.credit_amount
            Description = row.description |> Option.ofObj |> Option.defaultValue ""
        }

    let toJournal (row: JournalRow) (entries: JournalEntryRow seq) : Journal =
        {
            JournalId = Some row.journal_id
            JournalDate = row.journal_date
            Description = row.description
            FiscalYear = row.fiscal_year
            Entries = entries |> Seq.map toJournalEntry |> Seq.toList
        }

    interface IJournalRepository with
        member this.SaveAsync(journal: Journal) : Task<Journal> =
            task {
                use connection = createConnection()
                connection.Open()
                use transaction = connection.BeginTransaction()

                try
                    // 仕訳ヘッダーを挿入
                    let insertJournalSql = """
                        INSERT INTO journals (journal_date, description, fiscal_year)
                        VALUES (@JournalDate, @Description, @FiscalYear)
                        RETURNING journal_id
                    """
                    let! journalId = connection.QuerySingleAsync<int>(
                        insertJournalSql,
                        {| JournalDate = journal.JournalDate
                           Description = journal.Description
                           FiscalYear = journal.FiscalYear |},
                        transaction)

                    // 仕訳明細を挿入
                    let insertEntrySql = """
                        INSERT INTO journal_entries (journal_id, account_code, debit_amount, credit_amount, description)
                        VALUES (@JournalId, @AccountCode, @DebitAmount, @CreditAmount, @Description)
                    """
                    for entry in journal.Entries do
                        let! _ = connection.ExecuteAsync(
                            insertEntrySql,
                            {| JournalId = journalId
                               AccountCode = entry.AccountCode
                               DebitAmount = entry.DebitAmount
                               CreditAmount = entry.CreditAmount
                               Description = entry.Description |},
                            transaction)
                        ()

                    transaction.Commit()

                    return { journal with JournalId = Some journalId }
                with
                | ex ->
                    transaction.Rollback()
                    return raise ex
            }

        member this.GetByIdAsync(journalId: int) : Task<Journal option> =
            task {
                use connection = createConnection()

                let journalSql = """
                    SELECT journal_id, journal_date, description, fiscal_year, created_at, updated_at
                    FROM journals
                    WHERE journal_id = @JournalId
                """
                let! journalRows = connection.QueryAsync<JournalRow>(journalSql, {| JournalId = journalId |})
                let journalRow = journalRows |> Seq.tryHead

                match journalRow with
                | None -> return None
                | Some row ->
                    let entriesSql = """
                        SELECT entry_id, journal_id, account_code, debit_amount, credit_amount, description
                        FROM journal_entries
                        WHERE journal_id = @JournalId
                    """
                    let! entries = connection.QueryAsync<JournalEntryRow>(entriesSql, {| JournalId = journalId |})
                    return Some (toJournal row entries)
            }

        member this.GetByFiscalYearAsync(fiscalYear: int) : Task<Journal list> =
            task {
                use connection = createConnection()

                let journalSql = """
                    SELECT journal_id, journal_date, description, fiscal_year, created_at, updated_at
                    FROM journals
                    WHERE fiscal_year = @FiscalYear
                    ORDER BY journal_date, journal_id
                """
                let! journalRows = connection.QueryAsync<JournalRow>(journalSql, {| FiscalYear = fiscalYear |})

                let journalIds = journalRows |> Seq.map (fun r -> r.journal_id) |> Seq.toArray
                if journalIds.Length = 0 then
                    return []
                else
                    let entriesSql = """
                        SELECT entry_id, journal_id, account_code, debit_amount, credit_amount, description
                        FROM journal_entries
                        WHERE journal_id = ANY(@JournalIds)
                    """
                    let! entries = connection.QueryAsync<JournalEntryRow>(entriesSql, {| JournalIds = journalIds |})

                    let entriesMap =
                        entries
                        |> Seq.groupBy (fun e -> e.journal_id)
                        |> Map.ofSeq

                    return journalRows
                        |> Seq.map (fun row ->
                            let journalEntries = entriesMap |> Map.tryFind row.journal_id |> Option.defaultValue Seq.empty
                            toJournal row journalEntries)
                        |> Seq.toList
            }
