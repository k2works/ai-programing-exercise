namespace AccountingSystem.Infrastructure.Persistence.Repositories

open System
open System.Threading.Tasks
open Npgsql
open Dapper
open AccountingSystem.Application.Port.Out
open AccountingSystem.Infrastructure.Persistence.DAO

/// <summary>
/// 仕訳 Read Model リポジトリ実装（Dapper）
/// </summary>
type JournalEntryReadModelRepository(connection: NpgsqlConnection) =

    /// エンティティをドメインモデルに変換
    let toReadModel (entity: JournalEntryReadModelEntity) : JournalEntryReadModel =
        {
            Id = entity.id
            EntryDate = entity.entry_date
            Description = entity.description
            Status = entity.status
            Deleted = entity.deleted
            CreatedAt = entity.created_at
            UpdatedAt = entity.updated_at
            ApprovedBy = if isNull entity.approved_by then None else Some entity.approved_by
            ApprovalComment = if isNull entity.approval_comment then None else Some entity.approval_comment
        }

    /// 明細エンティティをドメインモデルに変換
    let toLineReadModel (entity: JournalEntryLineReadModelEntity) : JournalEntryLineReadModel =
        {
            Id = entity.id
            JournalEntryId = entity.journal_entry_id
            AccountCode = entity.account_code
            DebitCredit = entity.debit_credit
            Amount = entity.amount
        }

    interface IJournalEntryReadModelRepository with
        member _.InsertJournalEntryAsync
            (id: string)
            (entryDate: DateTime)
            (description: string)
            (status: string)
            (deleted: bool)
            (createdAt: DateTime)
            (updatedAt: DateTime)
            (approvedBy: string option)
            (approvalComment: string option)
            : Task<unit> =
            task {
                let sql = """
                    INSERT INTO journal_entry_read_model (
                        id, entry_date, description, status, deleted,
                        created_at, updated_at, approved_by, approval_comment
                    ) VALUES (
                        @Id, @EntryDate, @Description, @Status, @Deleted,
                        @CreatedAt, @UpdatedAt, @ApprovedBy, @ApprovalComment
                    )
                """

                let! _ = connection.ExecuteAsync(sql, {|
                    Id = id
                    EntryDate = entryDate
                    Description = description
                    Status = status
                    Deleted = deleted
                    CreatedAt = createdAt
                    UpdatedAt = updatedAt
                    ApprovedBy = approvedBy |> Option.toObj
                    ApprovalComment = approvalComment |> Option.toObj
                |})

                return ()
            }

        member _.InsertJournalEntryLineAsync
            (journalEntryId: string)
            (accountCode: string)
            (debitCredit: string)
            (amount: decimal)
            : Task<unit> =
            task {
                let sql = """
                    INSERT INTO journal_entry_line_read_model (
                        journal_entry_id, account_code, debit_credit, amount
                    ) VALUES (
                        @JournalEntryId, @AccountCode, @DebitCredit, @Amount
                    )
                """

                let! _ = connection.ExecuteAsync(sql, {|
                    JournalEntryId = journalEntryId
                    AccountCode = accountCode
                    DebitCredit = debitCredit
                    Amount = amount
                |})

                return ()
            }

        member _.UpdateJournalEntryStatusAsync
            (id: string)
            (status: string)
            (updatedAt: DateTime)
            (approvedBy: string)
            (approvalComment: string)
            : Task<unit> =
            task {
                let sql = """
                    UPDATE journal_entry_read_model
                    SET status = @Status,
                        updated_at = @UpdatedAt,
                        approved_by = @ApprovedBy,
                        approval_comment = @ApprovalComment
                    WHERE id = @Id
                """

                let! _ = connection.ExecuteAsync(sql, {|
                    Id = id
                    Status = status
                    UpdatedAt = updatedAt
                    ApprovedBy = approvedBy
                    ApprovalComment = approvalComment
                |})

                return ()
            }

        member _.MarkAsDeletedAsync (id: string) (updatedAt: DateTime) : Task<unit> =
            task {
                let sql = """
                    UPDATE journal_entry_read_model
                    SET deleted = true, updated_at = @UpdatedAt
                    WHERE id = @Id
                """

                let! _ = connection.ExecuteAsync(sql, {| Id = id; UpdatedAt = updatedAt |})

                return ()
            }

        member _.SelectByIdAsync (id: string) : Task<JournalEntryReadModel option> =
            task {
                let sql = """
                    SELECT id, entry_date, description, status, deleted,
                           created_at, updated_at, approved_by, approval_comment
                    FROM journal_entry_read_model
                    WHERE id = @Id
                """

                let! entity = connection.QuerySingleOrDefaultAsync<JournalEntryReadModelEntity>(sql, {| Id = id |})

                return if isNull (box entity) then None else Some (toReadModel entity)
            }

        member _.SelectByDateRangeAsync (startDate: DateTime) (endDate: DateTime) : Task<JournalEntryReadModel list> =
            task {
                let sql = """
                    SELECT id, entry_date, description, status, deleted,
                           created_at, updated_at, approved_by, approval_comment
                    FROM journal_entry_read_model
                    WHERE entry_date BETWEEN @StartDate AND @EndDate
                      AND deleted = false
                    ORDER BY entry_date DESC
                """

                let! entities = connection.QueryAsync<JournalEntryReadModelEntity>(sql, {|
                    StartDate = startDate
                    EndDate = endDate
                |})

                return entities |> Seq.map toReadModel |> Seq.toList
            }

        member _.SelectLinesByJournalEntryIdAsync (journalEntryId: string) : Task<JournalEntryLineReadModel list> =
            task {
                let sql = """
                    SELECT id, journal_entry_id, account_code, debit_credit, amount
                    FROM journal_entry_line_read_model
                    WHERE journal_entry_id = @JournalEntryId
                """

                let! entities = connection.QueryAsync<JournalEntryLineReadModelEntity>(sql, {|
                    JournalEntryId = journalEntryId
                |})

                return entities |> Seq.map toLineReadModel |> Seq.toList
            }

        member _.SelectAllAsync () : Task<JournalEntryReadModel list> =
            task {
                let sql = """
                    SELECT id, entry_date, description, status, deleted,
                           created_at, updated_at, approved_by, approval_comment
                    FROM journal_entry_read_model
                    WHERE deleted = false
                    ORDER BY entry_date DESC
                """

                let! entities = connection.QueryAsync<JournalEntryReadModelEntity>(sql)

                return entities |> Seq.map toReadModel |> Seq.toList
            }
