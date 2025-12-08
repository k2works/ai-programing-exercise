namespace ManagementAccounting.Infrastructure.Messaging

open System.Threading.Tasks
open MassTransit
open Microsoft.Extensions.Logging
open Shared.Contracts.Events
open ManagementAccounting.Application.Ports.In

/// <summary>
/// 仕訳作成イベントのコンシューマー
/// </summary>
type JournalCreatedEventConsumer(
    eventHandler: IJournalEventHandler,
    logger: ILogger<JournalCreatedEventConsumer>) =

    interface IConsumer<JournalCreatedEvent> with
        member _.Consume(context: ConsumeContext<JournalCreatedEvent>) : Task =
            task {
                let event = context.Message
                logger.LogDebug(
                    "仕訳作成イベントを受信: JournalId={JournalId}, FiscalYear={FiscalYear}",
                    event.JournalId,
                    event.FiscalYear
                )

                do! eventHandler.HandleJournalCreatedAsync(event)
            }

/// <summary>
/// 仕訳更新イベントのコンシューマー
/// </summary>
type JournalUpdatedEventConsumer(
    eventHandler: IJournalEventHandler,
    logger: ILogger<JournalUpdatedEventConsumer>) =

    interface IConsumer<JournalUpdatedEvent> with
        member _.Consume(context: ConsumeContext<JournalUpdatedEvent>) : Task =
            task {
                let event = context.Message
                logger.LogDebug(
                    "仕訳更新イベントを受信: JournalId={JournalId}, FiscalYear={FiscalYear}",
                    event.JournalId,
                    event.FiscalYear
                )

                do! eventHandler.HandleJournalUpdatedAsync(event)
            }

/// <summary>
/// 仕訳削除イベントのコンシューマー
/// </summary>
type JournalDeletedEventConsumer(
    eventHandler: IJournalEventHandler,
    logger: ILogger<JournalDeletedEventConsumer>) =

    interface IConsumer<JournalDeletedEvent> with
        member _.Consume(context: ConsumeContext<JournalDeletedEvent>) : Task =
            task {
                let event = context.Message
                logger.LogDebug(
                    "仕訳削除イベントを受信: JournalId={JournalId}, FiscalYear={FiscalYear}",
                    event.JournalId,
                    event.FiscalYear
                )

                do! eventHandler.HandleJournalDeletedAsync(event)
            }
