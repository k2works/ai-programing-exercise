namespace AccountingSystem.Infrastructure.Adapters

open System
open System.Threading.Tasks
open Npgsql
open AccountingSystem.Application.Port.Out
open AccountingSystem.Infrastructure.Persistence.Repositories

/// <summary>
/// 仕訳 Read Model リポジトリアダプター（Port/Out の実装）
/// 接続文字列を受け取り、内部でコネクションを管理
/// </summary>
type JournalEntryReadModelRepositoryAdapter(connectionString: string) =

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
                use connection = new NpgsqlConnection(connectionString)
                do! connection.OpenAsync()
                let repository = JournalEntryReadModelRepository(connection)
                return! (repository :> IJournalEntryReadModelRepository).InsertJournalEntryAsync
                    id entryDate description status deleted createdAt updatedAt approvedBy approvalComment
            }

        member _.InsertJournalEntryLineAsync
            (journalEntryId: string)
            (accountCode: string)
            (debitCredit: string)
            (amount: decimal)
            : Task<unit> =
            task {
                use connection = new NpgsqlConnection(connectionString)
                do! connection.OpenAsync()
                let repository = JournalEntryReadModelRepository(connection)
                return! (repository :> IJournalEntryReadModelRepository).InsertJournalEntryLineAsync
                    journalEntryId accountCode debitCredit amount
            }

        member _.UpdateJournalEntryStatusAsync
            (id: string)
            (status: string)
            (updatedAt: DateTime)
            (approvedBy: string)
            (approvalComment: string)
            : Task<unit> =
            task {
                use connection = new NpgsqlConnection(connectionString)
                do! connection.OpenAsync()
                let repository = JournalEntryReadModelRepository(connection)
                return! (repository :> IJournalEntryReadModelRepository).UpdateJournalEntryStatusAsync
                    id status updatedAt approvedBy approvalComment
            }

        member _.MarkAsDeletedAsync (id: string) (updatedAt: DateTime) : Task<unit> =
            task {
                use connection = new NpgsqlConnection(connectionString)
                do! connection.OpenAsync()
                let repository = JournalEntryReadModelRepository(connection)
                return! (repository :> IJournalEntryReadModelRepository).MarkAsDeletedAsync id updatedAt
            }

        member _.SelectByIdAsync (id: string) : Task<JournalEntryReadModel option> =
            task {
                use connection = new NpgsqlConnection(connectionString)
                do! connection.OpenAsync()
                let repository = JournalEntryReadModelRepository(connection)
                return! (repository :> IJournalEntryReadModelRepository).SelectByIdAsync id
            }

        member _.SelectByDateRangeAsync (startDate: DateTime) (endDate: DateTime) : Task<JournalEntryReadModel list> =
            task {
                use connection = new NpgsqlConnection(connectionString)
                do! connection.OpenAsync()
                let repository = JournalEntryReadModelRepository(connection)
                return! (repository :> IJournalEntryReadModelRepository).SelectByDateRangeAsync startDate endDate
            }

        member _.SelectLinesByJournalEntryIdAsync (journalEntryId: string) : Task<JournalEntryLineReadModel list> =
            task {
                use connection = new NpgsqlConnection(connectionString)
                do! connection.OpenAsync()
                let repository = JournalEntryReadModelRepository(connection)
                return! (repository :> IJournalEntryReadModelRepository).SelectLinesByJournalEntryIdAsync journalEntryId
            }

        member _.SelectAllAsync () : Task<JournalEntryReadModel list> =
            task {
                use connection = new NpgsqlConnection(connectionString)
                do! connection.OpenAsync()
                let repository = JournalEntryReadModelRepository(connection)
                return! (repository :> IJournalEntryReadModelRepository).SelectAllAsync ()
            }
