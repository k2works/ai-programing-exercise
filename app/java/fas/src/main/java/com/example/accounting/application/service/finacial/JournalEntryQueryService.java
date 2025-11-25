package com.example.accounting.application.service.finacial;

import com.example.accounting.infrastructure.out.persistence.dao.JournalEntryReadModel;
import com.example.accounting.infrastructure.out.persistence.mapper.JournalEntryReadModelMapper;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDate;
import java.util.List;

/**
 * 仕訳クエリサービス（CQRS Query Side）
 */
@Service
@RequiredArgsConstructor
public class JournalEntryQueryService {
    private final JournalEntryReadModelMapper readModelMapper;

    /**
     * ID で仕訳を取得
     *
     * @param id 仕訳ID
     * @return 仕訳 Read Model
     */
    @Transactional(readOnly = true)
    public JournalEntryReadModel findById(String id) {
        JournalEntryReadModel result = readModelMapper.findById(id);
        if (result == null) {
            throw new IllegalArgumentException("仕訳が見つかりません: " + id);
        }
        return result;
    }

    /**
     * すべての仕訳を取得
     *
     * @return 仕訳リスト
     */
    @Transactional(readOnly = true)
    public List<JournalEntryReadModel> findAll() {
        return readModelMapper.findAll();
    }

    /**
     * 起票日で仕訳を取得
     *
     * @param entryDate 起票日
     * @return 仕訳リスト
     */
    @Transactional(readOnly = true)
    public List<JournalEntryReadModel> findByEntryDate(LocalDate entryDate) {
        return readModelMapper.findByEntryDate(entryDate);
    }

    /**
     * ステータスで仕訳を取得
     *
     * @param status ステータス
     * @return 仕訳リスト
     */
    @Transactional(readOnly = true)
    public List<JournalEntryReadModel> findByStatus(String status) {
        return readModelMapper.findByStatus(status);
    }

    /**
     * 削除されていない仕訳を取得
     *
     * @return 仕訳リスト
     */
    @Transactional(readOnly = true)
    public List<JournalEntryReadModel> findNotDeleted() {
        return readModelMapper.findNotDeleted();
    }
}
