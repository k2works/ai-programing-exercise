package com.example.accounting.application.port.in;

import com.example.accounting.domain.model.Journal;
import java.util.List;

/**
 * 仕訳ユースケース（Input Port）
 * ビジネスユースケースのインターフェース定義
 */
public interface JournalUseCase {

    /**
     * すべての仕訳を取得
     * @return 仕訳一覧
     */
    List<Journal> getAllJournals();

    /**
     * 仕訳番号で仕訳を取得
     * @param journalNo 仕訳番号
     * @return 仕訳
     */
    Journal getJournalByNo(String journalNo);

    /**
     * 仕訳を作成
     * @param journal 仕訳
     * @return 作成された仕訳
     */
    Journal createJournal(Journal journal);

    /**
     * 仕訳を更新
     * @param journalNo 仕訳番号
     * @param journal 仕訳
     * @return 更新された仕訳
     */
    Journal updateJournal(String journalNo, Journal journal);

    /**
     * 仕訳を削除
     * @param journalNo 仕訳番号
     */
    void deleteJournal(String journalNo);
}
