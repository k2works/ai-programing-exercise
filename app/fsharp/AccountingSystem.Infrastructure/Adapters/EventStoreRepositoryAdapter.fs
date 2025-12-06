namespace AccountingSystem.Infrastructure.Adapters

open System
open System.Threading.Tasks
open Npgsql
open AccountingSystem.Application.Port.Out
open AccountingSystem.Domain.Events
open AccountingSystem.Infrastructure.Persistence.Repositories

/// <summary>
/// イベントストアリポジトリアダプター（Port/Out の実装）
/// 接続文字列を受け取り、内部でコネクションを管理
/// </summary>
type EventStoreRepositoryAdapter(connectionString: string) =

    interface IEventStoreRepository with
        member _.SaveAsync
            (aggregateId: string)
            (aggregateType: string)
            (events: JournalEntryEvent list)
            (expectedVersion: int)
            (userId: string)
            (correlationId: string option)
            : Task<Result<unit, string>> =
            task {
                use connection = new NpgsqlConnection(connectionString)
                do! connection.OpenAsync()
                let repository = EventStoreRepository(connection)
                return! repository.SaveAsync aggregateId aggregateType events expectedVersion userId correlationId
            }

        member _.GetEventsAsync (aggregateId: string) : Task<JournalEntryEvent list> =
            task {
                use connection = new NpgsqlConnection(connectionString)
                do! connection.OpenAsync()
                let repository = EventStoreRepository(connection)
                return! repository.GetEventsAsync aggregateId
            }

        member _.GetEventsUntilAsync (aggregateId: string) (pointInTime: DateTime) : Task<JournalEntryEvent list> =
            task {
                use connection = new NpgsqlConnection(connectionString)
                do! connection.OpenAsync()
                let repository = EventStoreRepository(connection)
                return! repository.GetEventsUntilAsync aggregateId pointInTime
            }

        member _.GetCurrentVersionAsync (aggregateId: string) : Task<int> =
            task {
                use connection = new NpgsqlConnection(connectionString)
                do! connection.OpenAsync()
                let repository = EventStoreRepository(connection)
                return! repository.GetCurrentVersionAsync aggregateId
            }

        member _.GetAggregateIdsAsync (aggregateType: string) : Task<string list> =
            task {
                use connection = new NpgsqlConnection(connectionString)
                do! connection.OpenAsync()
                let repository = EventStoreRepository(connection)
                return! repository.GetAggregateIdsAsync aggregateType
            }

        member _.GetEventsSinceVersionAsync (aggregateId: string) (sinceVersion: int) : Task<JournalEntryEvent list> =
            task {
                use connection = new NpgsqlConnection(connectionString)
                do! connection.OpenAsync()
                let repository = EventStoreRepository(connection)
                return! repository.GetEventsSinceVersionAsync aggregateId sinceVersion
            }
