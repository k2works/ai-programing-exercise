package com.example.accounting.infrastructure.out.persistence.mapper;

import com.example.accounting.infrastructure.out.persistence.dao.AutoJournalManagement;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

/**
 * 自動仕訳管理マッパー
 */
@Mapper
public interface AutoJournalManagementMapper {

    /**
     * 全件取得
     */
    List<AutoJournalManagement> findAll();

    /**
     * ID検索
     */
    AutoJournalManagement findById(@Param("id") Long id);

    /**
     * ソーステーブル名で検索
     */
    AutoJournalManagement findBySourceTableName(@Param("sourceTableName") String sourceTableName);

    /**
     * 挿入
     */
    void insert(AutoJournalManagement management);

    /**
     * 更新
     */
    void update(AutoJournalManagement management);

    /**
     * 削除
     */
    void delete(@Param("id") Long id);
}
