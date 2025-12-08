namespace FinancialAccounting.Infrastructure.Messaging

open System
open System.Threading.Tasks
open MassTransit
open FinancialAccounting.Domain.Entities
open FinancialAccounting.Application.Ports.Out
open Shared.Contracts.Events

/// <summary>
/// MassTransit を使用した仕訳イベントの発行者
/// </summary>
type JournalEventPublisher(publishEndpoint: IPublishEndpoint) =

    interface IJournalEventPublisher with
        member _.PublishJournalCreatedAsync(journal: Journal) : Task =
            task {
                let totalAmount =
                    journal.Entries
                    |> List.sumBy (fun e -> e.DebitAmount)

                let event: JournalCreatedEvent = {
                    JournalId = journal.JournalId |> Option.defaultValue 0
                    FiscalYear = journal.FiscalYear
                    JournalDate = journal.JournalDate
                    Description = journal.Description
                    TotalAmount = totalAmount
                    CreatedAt = DateTime.UtcNow
                }

                do! publishEndpoint.Publish(event)
            }

        member _.PublishJournalUpdatedAsync(journal: Journal) : Task =
            task {
                let entries =
                    journal.Entries
                    |> List.map (fun e -> {
                        AccountCode = e.AccountCode
                        DebitAmount = e.DebitAmount
                        CreditAmount = e.CreditAmount
                        Description = e.Description
                    })

                let event: JournalUpdatedEvent = {
                    JournalId = journal.JournalId |> Option.defaultValue 0
                    FiscalYear = journal.FiscalYear
                    JournalDate = journal.JournalDate
                    Description = journal.Description
                    Entries = entries
                    UpdatedAt = DateTime.UtcNow
                }

                do! publishEndpoint.Publish(event)
            }

        member _.PublishJournalDeletedAsync(journalId: int, fiscalYear: int) : Task =
            task {
                let event: JournalDeletedEvent = {
                    JournalId = journalId
                    FiscalYear = fiscalYear
                    DeletedAt = DateTime.UtcNow
                }

                do! publishEndpoint.Publish(event)
            }
