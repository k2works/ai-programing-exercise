package com.example.accounting.application.service;

import com.example.accounting.application.exception.JournalNotFoundException;
import com.example.accounting.application.port.in.JournalUseCase;
import com.example.accounting.application.port.out.JournalRepository;
import com.example.accounting.domain.model.Journal;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

/**
 * 仕訳サービス（Application Service）
 * Input Port の実装としてビジネスロジックを提供
 */
@Service
@Transactional
public class JournalService implements JournalUseCase {

    private final JournalRepository journalRepository;

    public JournalService(JournalRepository journalRepository) {
        this.journalRepository = journalRepository;
    }

    @Override
    @Transactional(readOnly = true)
    public List<Journal> getAllJournals() {
        return journalRepository.findAll();
    }

    @Override
    @Transactional(readOnly = true)
    public Journal getJournalByNo(String journalNo) {
        return journalRepository.findByJournalNo(journalNo)
                .orElseThrow(() -> new JournalNotFoundException(
                        "仕訳番号 " + journalNo + " が見つかりません"));
    }

    @Override
    public Journal createJournal(Journal journal) {
        // ビジネスルール：仕訳番号の重複チェック
        journalRepository.findByJournalNo(journal.getJournalNo()).ifPresent(existing -> {
            throw new IllegalStateException(
                    "仕訳番号 " + journal.getJournalNo() + " は既に存在します");
        });

        return journalRepository.save(journal);
    }

    @Override
    public Journal updateJournal(String journalNo, Journal journal) {
        // 存在チェック
        getJournalByNo(journalNo);

        // 仕訳番号変更チェック
        if (!journalNo.equals(journal.getJournalNo())) {
            throw new IllegalArgumentException("仕訳番号は変更できません");
        }

        return journalRepository.save(journal);
    }

    @Override
    public void deleteJournal(String journalNo) {
        // 存在チェック
        getJournalByNo(journalNo);

        journalRepository.deleteByJournalNo(journalNo);
    }
}
