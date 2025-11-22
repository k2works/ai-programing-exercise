package com.example.accounting.application;

import com.example.accounting.domain.Journal;
import com.example.accounting.domain.JournalEntry;
import com.example.accounting.port.in.CreateJournalUseCase;
import com.example.accounting.port.in.JournalEntryRequest;
import com.example.accounting.port.out.JournalRepository;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDate;
import java.util.List;

@Service
public class CreateJournalService implements CreateJournalUseCase {

    private final JournalRepository journalRepository;

    public CreateJournalService(JournalRepository journalRepository) {
        this.journalRepository = journalRepository;
    }

    @Override
    @Transactional
    public Journal createJournal(LocalDate journalDate, String description, Integer fiscalYear, List<JournalEntryRequest> entries) {
        // ドメインエンティティの作成
        Journal journal = new Journal(journalDate, description, fiscalYear);

        // 仕訳明細の追加
        for (JournalEntryRequest entryRequest : entries) {
            JournalEntry entry = new JournalEntry(
                entryRequest.getAccountCode(),
                entryRequest.getDebitAmount(),
                entryRequest.getCreditAmount(),
                entryRequest.getDescription()
            );
            journal.addEntry(entry);
        }

        // ビジネスルールの検証（貸借一致）
        journal.validateBalance();

        // リポジトリに保存
        return journalRepository.save(journal);
    }
}
