namespace AccountingSystem.Infrastructure.Adapters

open System.Threading.Tasks
open Npgsql
open AccountingSystem.Domain.Aggregates
open AccountingSystem.Application.Port.Out
open AccountingSystem.Infrastructure.Persistence.Repositories

/// <summary>
/// スナップショットリポジトリアダプター
/// コネクション管理を行い、ISnapshotRepository を実装
/// </summary>
type SnapshotRepositoryAdapter(connectionString: string) =

    interface ISnapshotRepository with

        member _.SaveSnapshotAsync
            (aggregateId: string)
            (aggregateType: string)
            (aggregate: JournalEntryAggregate)
            : Task<Result<unit, string>> =
            task {
                use connection = new NpgsqlConnection(connectionString)
                do! connection.OpenAsync()
                let repository = SnapshotRepository(connection)
                return! repository.SaveSnapshotAsync aggregateId aggregateType aggregate
            }

        member _.GetSnapshotAsync
            (aggregateId: string)
            (aggregateType: string)
            : Task<JournalEntryAggregate option> =
            task {
                use connection = new NpgsqlConnection(connectionString)
                do! connection.OpenAsync()
                let repository = SnapshotRepository(connection)
                return! repository.GetSnapshotAsync aggregateId aggregateType
            }

        member _.DeleteSnapshotAsync
            (aggregateId: string)
            (aggregateType: string)
            : Task<unit> =
            task {
                use connection = new NpgsqlConnection(connectionString)
                do! connection.OpenAsync()
                let repository = SnapshotRepository(connection)
                return! repository.DeleteSnapshotAsync aggregateId aggregateType
            }

        member _.GetSnapshotVersionAsync
            (aggregateId: string)
            (aggregateType: string)
            : Task<int> =
            task {
                use connection = new NpgsqlConnection(connectionString)
                do! connection.OpenAsync()
                let repository = SnapshotRepository(connection)
                return! repository.GetSnapshotVersionAsync aggregateId aggregateType
            }
