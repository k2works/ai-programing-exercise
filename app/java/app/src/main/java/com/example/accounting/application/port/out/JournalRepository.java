package com.example.accounting.application.port.out;

import com.example.accounting.application.model.Journal;
import java.util.List;
import java.util.Optional;

/**
 * 仕訳リポジトリ（Output Port）
 * データアクセスの抽象化
 */
public interface JournalRepository {

    /**
     * すべての仕訳を取得
     * @return 仕訳リスト
     */
    List<Journal> findAll();

    /**
     * 仕訳番号で仕訳を検索
     * @param journalNo 仕訳番号
     * @return 仕訳（存在しない場合は Empty）
     */
    Optional<Journal> findByJournalNo(String journalNo);

    /**
     * 仕訳を保存（作成または更新）
     * @param journal 仕訳
     * @return 保存された仕訳
     */
    Journal save(Journal journal);

    /**
     * 仕訳を削除
     * @param journalNo 仕訳番号
     */
    void deleteByJournalNo(String journalNo);
}
