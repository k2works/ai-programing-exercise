package com.example.accounting.infrastructure.out.persistence.mapper;

import com.example.accounting.infrastructure.out.persistence.entity.AutoJournalPatternItem;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

/**
 * 自動仕訳パターン明細マッパー
 */
@Mapper
public interface AutoJournalPatternItemMapper {

    /**
     * 全件取得
     */
    List<AutoJournalPatternItem> findAll();

    /**
     * ID検索
     */
    AutoJournalPatternItem findById(@Param("id") Long id);

    /**
     * パターンIDで検索
     */
    List<AutoJournalPatternItem> findByPatternId(@Param("patternId") Long patternId);

    /**
     * 挿入
     */
    void insert(AutoJournalPatternItem item);

    /**
     * 更新
     */
    void update(AutoJournalPatternItem item);

    /**
     * 削除
     */
    void delete(@Param("id") Long id);

    /**
     * パターンIDで削除
     */
    void deleteByPatternId(@Param("patternId") Long patternId);
}
