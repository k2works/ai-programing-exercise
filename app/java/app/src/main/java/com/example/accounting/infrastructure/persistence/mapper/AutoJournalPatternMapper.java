package com.example.accounting.infrastructure.persistence.mapper;

import com.example.accounting.infrastructure.persistence.entity.AutoJournalPattern;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

/**
 * 自動仕訳パターンマッパー
 */
@Mapper
public interface AutoJournalPatternMapper {

    /**
     * 全件取得
     */
    List<AutoJournalPattern> findAll();

    /**
     * ID検索
     */
    AutoJournalPattern findById(@Param("id") Long id);

    /**
     * パターンコード検索
     */
    AutoJournalPattern findByPatternCode(@Param("patternCode") String patternCode);

    /**
     * ソーステーブル名で検索
     */
    List<AutoJournalPattern> findBySourceTableName(@Param("sourceTableName") String sourceTableName);

    /**
     * 有効なパターンを取得
     */
    List<AutoJournalPattern> findActivePatterns();

    /**
     * 挿入
     */
    void insert(AutoJournalPattern pattern);

    /**
     * 更新
     */
    void update(AutoJournalPattern pattern);

    /**
     * 削除
     */
    void delete(@Param("id") Long id);
}
