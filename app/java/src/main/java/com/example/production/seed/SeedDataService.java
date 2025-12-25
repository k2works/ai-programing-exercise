package com.example.production.seed;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDate;

/**
 * Seed データ投入サービス
 *
 * E 精密工業株式会社（架空）のテストデータを投入する
 */
@Service
@RequiredArgsConstructor
@Slf4j
public class SeedDataService {

    private final MasterDataSeeder masterDataSeeder;
    private final TransactionDataSeeder transactionDataSeeder;

    /**
     * すべての Seed データを投入する
     */
    @Transactional
    public void seedAll() {
        log.info("========================================");
        log.info("生産管理システム Seed データ投入開始");
        log.info("========================================");

        LocalDate effectiveDate = LocalDate.of(2025, 1, 1);

        // 既存データの削除
        cleanAllData();

        // マスタデータの投入
        masterDataSeeder.seedAll(effectiveDate);

        // トランザクションデータの投入
        transactionDataSeeder.seedAll(effectiveDate);

        log.info("========================================");
        log.info("生産管理システム Seed データ投入完了!");
        log.info("========================================");
    }

    /**
     * 既存データをすべて削除する
     */
    private void cleanAllData() {
        log.info("既存データを削除中...");

        // トランザクションデータから削除（外部キー制約のため逆順）
        transactionDataSeeder.deleteAll();

        // マスタデータを削除
        masterDataSeeder.deleteAll();

        log.info("既存データ削除完了");
    }
}
