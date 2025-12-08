namespace ManagementAccounting.Application.UseCases

open System.Threading.Tasks
open Microsoft.Extensions.Logging
open Shared.Contracts.Events
open ManagementAccounting.Application.Ports.In
open ManagementAccounting.Application.Ports.Out

/// <summary>
/// 仕訳イベントハンドラ実装
/// 財務会計サービスからのイベントを処理し、キャッシュを無効化する
/// </summary>
type JournalEventHandler(
    cacheRepository: IFinancialAnalysisCacheRepository,
    logger: ILogger<JournalEventHandler>) =

    interface IJournalEventHandler with
        member _.HandleJournalCreatedAsync(event: JournalCreatedEvent) : Task<unit> =
            task {
                logger.LogInformation(
                    "仕訳作成イベントを処理: JournalId={JournalId}, FiscalYear={FiscalYear}, TotalAmount={TotalAmount}",
                    event.JournalId,
                    event.FiscalYear,
                    event.TotalAmount
                )

                // 該当会計年度のキャッシュを無効化
                let! deleted = cacheRepository.DeleteByFiscalYearAsync(event.FiscalYear)
                if deleted then
                    logger.LogInformation(
                        "会計年度 {FiscalYear} のキャッシュを無効化しました",
                        event.FiscalYear
                    )
                else
                    logger.LogDebug(
                        "会計年度 {FiscalYear} のキャッシュは存在しませんでした",
                        event.FiscalYear
                    )
            }

        member _.HandleJournalUpdatedAsync(event: JournalUpdatedEvent) : Task<unit> =
            task {
                logger.LogInformation(
                    "仕訳更新イベントを処理: JournalId={JournalId}, FiscalYear={FiscalYear}, EntriesCount={EntriesCount}",
                    event.JournalId,
                    event.FiscalYear,
                    event.Entries.Length
                )

                // 該当会計年度のキャッシュを無効化
                let! deleted = cacheRepository.DeleteByFiscalYearAsync(event.FiscalYear)
                if deleted then
                    logger.LogInformation(
                        "会計年度 {FiscalYear} のキャッシュを無効化しました",
                        event.FiscalYear
                    )
                else
                    logger.LogDebug(
                        "会計年度 {FiscalYear} のキャッシュは存在しませんでした",
                        event.FiscalYear
                    )
            }

        member _.HandleJournalDeletedAsync(event: JournalDeletedEvent) : Task<unit> =
            task {
                logger.LogInformation(
                    "仕訳削除イベントを処理: JournalId={JournalId}, FiscalYear={FiscalYear}",
                    event.JournalId,
                    event.FiscalYear
                )

                // 該当会計年度のキャッシュを無効化
                let! deleted = cacheRepository.DeleteByFiscalYearAsync(event.FiscalYear)
                if deleted then
                    logger.LogInformation(
                        "会計年度 {FiscalYear} のキャッシュを無効化しました",
                        event.FiscalYear
                    )
                else
                    logger.LogDebug(
                        "会計年度 {FiscalYear} のキャッシュは存在しませんでした",
                        event.FiscalYear
                    )
            }
