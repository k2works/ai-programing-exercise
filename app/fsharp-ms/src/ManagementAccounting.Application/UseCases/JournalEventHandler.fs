namespace ManagementAccounting.Application.UseCases

open System
open System.Threading.Tasks
open Microsoft.Extensions.Logging
open Shared.Contracts.Events
open ManagementAccounting.Domain.Models
open ManagementAccounting.Application.Ports.In
open ManagementAccounting.Application.Ports.Out

/// <summary>
/// 仕訳イベントハンドラ実装
/// 財務会計サービスからのイベントを処理し、キャッシュを更新する
/// </summary>
type JournalEventHandler(
    cacheRepository: IFinancialAnalysisCacheRepository,
    financialDataPort: IFinancialDataPort,
    logger: ILogger<JournalEventHandler>) =

    /// キャッシュを再構築する共通処理
    let rebuildCacheAsync (fiscalYear: int) =
        task {
            try
                // 財務会計サービスからデータを取得
                let! data = financialDataPort.FetchFinancialDataByFiscalYearAsync(fiscalYear)

                // 財務比率を計算
                let ratios = FinancialRatios.calculate data

                let now = DateTime.UtcNow

                // キャッシュに保存
                let cache = {
                    Id = None
                    FiscalYear = fiscalYear
                    Data = data
                    Ratios = ratios
                    CachedAt = now
                }
                let! _ = cacheRepository.SaveAsync(cache)

                logger.LogInformation(
                    "会計年度 {FiscalYear} のキャッシュを再構築しました",
                    fiscalYear
                )
            with
            | ex ->
                logger.LogWarning(
                    ex,
                    "会計年度 {FiscalYear} のキャッシュ再構築に失敗しました: {Message}",
                    fiscalYear,
                    ex.Message
                )
        }

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

                // キャッシュを再構築
                do! rebuildCacheAsync event.FiscalYear
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

                // キャッシュを再構築
                do! rebuildCacheAsync event.FiscalYear
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

                // キャッシュを再構築
                do! rebuildCacheAsync event.FiscalYear
            }
