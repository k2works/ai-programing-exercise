package com.example.accounting.mapper;

import com.example.accounting.entity.Journal;
import com.example.accounting.entity.JournalDetail;
import com.example.accounting.entity.JournalDetailItem;
import org.apache.ibatis.annotations.Param;

import java.util.List;

/**
 * 仕訳 Mapper インターフェース
 */
public interface JournalMapper {
    /**
     * 仕訳を登録（ヘッダー）
     */
    void insertJournal(Journal journal);

    /**
     * 仕訳明細を登録
     */
    void insertJournalDetail(JournalDetail detail);

    /**
     * 仕訳貸借明細を登録
     */
    void insertJournalDetailItem(JournalDetailItem item);

    /**
     * 仕訳を取得（明細・貸借明細を含む）
     */
    Journal findByJournalNo(@Param("journalNo") String journalNo);

    /**
     * 仕訳明細を取得
     */
    List<JournalDetail> findDetailsByJournalNo(@Param("journalNo") String journalNo);

    /**
     * 仕訳貸借明細を取得
     */
    List<JournalDetailItem> findItemsByJournalNoAndLine(@Param("journalNo") String journalNo,
                                                          @Param("lineNumber") Integer lineNumber);

    /**
     * 仕訳を削除（CASCADE により明細・貸借明細も削除される）
     */
    void deleteByJournalNo(@Param("journalNo") String journalNo);

    /**
     * 全仕訳を取得
     */
    List<Journal> findAll();
}
