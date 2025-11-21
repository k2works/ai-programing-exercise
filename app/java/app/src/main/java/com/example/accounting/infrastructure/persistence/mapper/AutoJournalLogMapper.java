package com.example.accounting.infrastructure.persistence.mapper;

import com.example.accounting.infrastructure.persistence.entity.AutoJournalLog;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

/**
 * 自動仕訳実行ログマッパー
 */
@Mapper
public interface AutoJournalLogMapper {

    /**
     * 全件取得
     */
    List<AutoJournalLog> findAll();

    /**
     * ID検索
     */
    AutoJournalLog findById(@Param("id") Long id);

    /**
     * パターンIDで検索
     */
    List<AutoJournalLog> findByPatternId(@Param("patternId") Long patternId);

    /**
     * ステータスで検索
     */
    List<AutoJournalLog> findByStatus(@Param("status") String status);

    /**
     * 挿入
     */
    void insert(AutoJournalLog log);

    /**
     * 更新
     */
    void update(AutoJournalLog log);

    /**
     * 削除
     */
    void delete(@Param("id") Long id);
}
