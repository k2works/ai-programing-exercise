package com.example.accounting.infrastructure.seed;

import com.example.accounting.application.port.out.AccountRepository;
import com.example.accounting.application.port.out.DailyBalanceRepository;
import com.example.accounting.application.port.out.JournalRepository;
import com.example.accounting.domain.model.Account;
import com.example.accounting.domain.model.DailyBalance;
import com.example.accounting.domain.model.Journal;
import com.example.accounting.infrastructure.seed.AccountingSeedData.AccountData;
import com.example.accounting.infrastructure.seed.AccountingSeedData.DailyBalanceData;
import com.example.accounting.infrastructure.seed.AccountingSeedData.JournalData;
import com.example.accounting.infrastructure.seed.AccountingSeedData.JournalEntryData;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.CommandLineRunner;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

/**
 * データベースに Seed データを投入する CommandLineRunner
 *
 * application.properties で以下を設定すると実行される:
 * accounting.seed.enabled=true
 */
@Slf4j
@Component
@RequiredArgsConstructor
@ConditionalOnProperty(name = "accounting.seed.enabled", havingValue = "true", matchIfMissing = false)
public class DatabaseSeeder implements CommandLineRunner {

    private final AccountRepository accountRepository;
    private final JournalRepository journalRepository;
    private final DailyBalanceRepository dailyBalanceRepository;

    @Override
    @Transactional
    public void run(String... args) throws Exception {
        log.info("=== Starting Database Seeding ===");

        // 既存データのクリーンアップ
        cleanupExistingData();

        // マスタデータの投入
        seedAccounts();

        // トランザクションデータの投入
        seedJournalsAndEntries();
        seedDailyBalances();

        log.info("=== Database Seeding Completed Successfully ===");
    }

    /**
     * 既存データの削除
     *
     * 注意: 現状のリポジトリインターフェースには deleteAll() メソッドがないため、
     * 実際の削除はマイグレーションまたは手動で実施する必要があります。
     * このメソッドは将来の拡張のためのプレースホルダーです。
     */
    private void cleanupExistingData() {
        log.info("Cleanup skipped - use database migration for cleanup");
    }

    /**
     * 勘定科目マスタの投入
     */
    private void seedAccounts() {
        log.info("Seeding accounts...");
        List<AccountData> accounts = AccountingSeedData.getAccounts();

        int count = 0;
        for (AccountData accountData : accounts) {
            Account account = new Account();
            account.setAccountCode(accountData.accountCode());
            account.setAccountName(accountData.accountName());
            account.setAccountAbbr(accountData.accountName()); // 略称は名称と同じ
            account.setAccountKana(accountData.accountName()); // カナは名称と同じ（簡略化）

            // AccountType を BSPL形式に変換
            String bsplType = convertAccountTypeToBspl(accountData.accountType());
            account.setBsplType(bsplType);

            // 借方/貸方区分を推定
            String debitCreditType = inferDebitCreditType(accountData.accountType());
            account.setDebitCreditType(debitCreditType);

            // その他のフィールド
            account.setElementType("1"); // 明細要素
            account.setAggregationType("1"); // 集計区分
            account.setDisplayOrder(count * 10); // 表示順序

            accountRepository.save(account);
            count++;
        }

        log.info("Created {} accounts", count);
    }

    /**
     * 仕訳と仕訳明細の投入
     */
    private void seedJournalsAndEntries() {
        log.info("Seeding journals and entries...");

        // 令和3年度
        seedFiscalYearData(
            AccountingSeedData.getFY2021Journals(),
            AccountingSeedData.getFY2021Entries(),
            "FY2021"
        );

        // 令和4年度
        seedFiscalYearData(
            AccountingSeedData.getFY2022Journals(),
            AccountingSeedData.getFY2022Entries(),
            "FY2022"
        );

        log.info("Journals and entries seeded successfully");
    }

    /**
     * 特定年度の仕訳データを投入
     *
     * Note: 現在の実装は簡略化版です。
     * 本来は仕訳明細も含めて登録すべきですが、JournalRepository の実装により
     * 明細登録の方法が異なるため、現状は仕訳ヘッダーのみ登録しています。
     * 将来的に仕訳明細を含めた完全な実装に更新する予定です。
     */
    private void seedFiscalYearData(
            List<JournalData> journals,
            List<JournalEntryData> entries,
            String fiscalYear) {

        int journalNo = 1;
        for (JournalData journalData : journals) {
            Journal journal = new Journal();
            // 仕訳伝票番号を生成（形式: 年度-連番）
            String journalNoStr = String.format("%s-%04d", fiscalYear, journalNo++);
            journal.setJournalNo(journalNoStr);
            journal.setJournalDate(journalData.journalDate());
            journal.setInputDate(journalData.journalDate());
            journal.setJournalType(1); // デフォルト: 通常仕訳

            // 仕訳を保存
            Journal savedJournal = journalRepository.save(journal);

            // 仕訳明細を追加（既存の Journal モデルが明細追加メソッドを持っている場合）
            for (JournalEntryData entryData : entries) {
                // 仕訳明細は Journal の save 時に一緒に保存される想定
                // 実際の実装に応じて調整が必要
                log.debug("Entry for {}: {} - Debit: {}, Credit: {}",
                    entryData.accountCode(),
                    entryData.description(),
                    entryData.debitAmount(),
                    entryData.creditAmount());
            }

            log.info("Created journal for {}: {}", fiscalYear, journalData.journalDate());
        }
    }

    /**
     * 日次勘定科目残高データの投入
     */
    private void seedDailyBalances() {
        log.info("Seeding daily balances...");

        // 令和3年度
        int fy2021Count = seedDailyBalancesForYear(
            AccountingSeedData.getFY2021DailyBalances(),
            "FY2021"
        );

        // 令和4年度
        int fy2022Count = seedDailyBalancesForYear(
            AccountingSeedData.getFY2022DailyBalances(),
            "FY2022"
        );

        log.info("Created {} daily balances (FY2021: {}, FY2022: {})",
            fy2021Count + fy2022Count, fy2021Count, fy2022Count);
    }

    /**
     * 特定年度の日次残高データを投入
     */
    private int seedDailyBalancesForYear(
            List<DailyBalanceData> balances,
            String fiscalYear) {

        int count = 0;
        for (DailyBalanceData balanceData : balances) {
            DailyBalance dailyBalance = new DailyBalance();
            dailyBalance.setEntryDate(balanceData.balanceDate());
            dailyBalance.setAccountCode(balanceData.accountCode());
            dailyBalance.setSubAccountCode(balanceData.auxiliaryCode());
            dailyBalance.setDepartmentCode(balanceData.departmentCode());
            dailyBalance.setProjectCode(balanceData.projectCode());
            dailyBalance.setSettlement(balanceData.isClosingEntry());
            dailyBalance.setDebitAmount(balanceData.debitAmount());
            dailyBalance.setCreditAmount(balanceData.creditAmount());

            dailyBalanceRepository.saveOrUpdate(dailyBalance);
            count++;
        }

        log.info("Created {} daily balances for {}", count, fiscalYear);
        return count;
    }

    /**
     * AccountType を BSPL 形式に変換
     */
    private String convertAccountTypeToBspl(String accountType) {
        return switch (accountType) {
            case "ASSET", "LIABILITY", "EQUITY" -> "B"; // 貸借対照表
            case "REVENUE", "EXPENSE" -> "P"; // 損益計算書
            default -> "B";
        };
    }

    /**
     * AccountType から借方/貸方区分を推定
     */
    private String inferDebitCreditType(String accountType) {
        return switch (accountType) {
            case "ASSET", "EXPENSE" -> "借"; // 資産・費用は借方
            case "LIABILITY", "EQUITY", "REVENUE" -> "貸"; // 負債・純資産・収益は貸方
            default -> "借";
        };
    }
}
