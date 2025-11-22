package com.example.accounting.application.service;

import com.example.accounting.application.exception.JournalNotFoundException;
import com.example.accounting.application.model.Journal;
import com.example.accounting.application.port.out.JournalRepository;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

/**
 * 仕訳サービス（Application Service）
 * ビジネスロジックを実装
 */
@Service
@Transactional
public class JournalService {

    private final JournalRepository journalRepository;

    public JournalService(JournalRepository journalRepository) {
        this.journalRepository = journalRepository;
    }

    /**
     * すべての仕訳を取得
     */
    @Transactional(readOnly = true)
    public List<Journal> getAllJournals() {
        return journalRepository.findAll();
    }

    /**
     * 仕訳番号で仕訳を取得
     */
    @Transactional(readOnly = true)
    public Journal getJournalByNo(String journalNo) {
        return journalRepository.findByJournalNo(journalNo)
                .orElseThrow(() -> new JournalNotFoundException(
                        "仕訳番号 " + journalNo + " が見つかりません"));
    }

    /**
     * 仕訳を作成
     */
    public Journal createJournal(Journal journal) {
        // ビジネスルール：仕訳番号の重複チェック
        journalRepository.findByJournalNo(journal.getJournalNo()).ifPresent(existing -> {
            throw new IllegalStateException(
                    "仕訳番号 " + journal.getJournalNo() + " は既に存在します");
        });

        return journalRepository.save(journal);
    }

    /**
     * 仕訳を更新
     */
    public Journal updateJournal(String journalNo, Journal journal) {
        // 存在チェック
        getJournalByNo(journalNo);

        // 仕訳番号変更チェック
        if (!journalNo.equals(journal.getJournalNo())) {
            throw new IllegalArgumentException("仕訳番号は変更できません");
        }

        return journalRepository.save(journal);
    }

    /**
     * 仕訳を削除
     */
    public void deleteJournal(String journalNo) {
        // 存在チェック
        getJournalByNo(journalNo);

        journalRepository.deleteByJournalNo(journalNo);
    }
}
