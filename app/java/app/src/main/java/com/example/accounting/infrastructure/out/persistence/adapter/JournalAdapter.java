package com.example.accounting.infrastructure.out.persistence.adapter;

import com.example.accounting.domain.model.Journal;
import com.example.accounting.domain.model.JournalEntry;
import com.example.accounting.domain.model.JournalLine;
import com.example.accounting.application.port.out.JournalRepository;
import com.example.accounting.infrastructure.out.persistence.entity.JournalDetail;
import com.example.accounting.infrastructure.out.persistence.entity.JournalDetailItem;
import com.example.accounting.infrastructure.out.persistence.mapper.JournalMapper;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

/**
 * 仕訳 Output Adapter
 * MyBatis Mapperとドメインモデルの橋渡し
 */
@Repository
public class JournalAdapter implements JournalRepository {

    private final JournalMapper journalMapper;

    public JournalAdapter(JournalMapper journalMapper) {
        this.journalMapper = journalMapper;
    }

    @Override
    public List<Journal> findAll() {
        return journalMapper.findAll().stream()
                .map(this::toDomainModel)
                .collect(Collectors.toList());
    }

    @Override
    public Optional<Journal> findByJournalNo(String journalNo) {
        com.example.accounting.infrastructure.out.persistence.entity.Journal entity =
                journalMapper.findByJournalNo(journalNo);
        return Optional.ofNullable(entity).map(this::toDomainModel);
    }

    @Override
    @Transactional
    public Journal save(Journal journal) {
        com.example.accounting.infrastructure.out.persistence.entity.Journal existingEntity =
                journalMapper.findByJournalNo(journal.getJournalNo());

        if (existingEntity != null) {
            // 既存の仕訳を削除してから再作成
            journalMapper.deleteByJournalNo(journal.getJournalNo());
        }

        // ヘッダー登録
        com.example.accounting.infrastructure.out.persistence.entity.Journal entity = toEntity(journal);
        journalMapper.insertJournal(entity);

        // 明細登録
        for (JournalEntry entry : journal.getEntries()) {
            JournalDetail detail = toDetailEntity(journal.getJournalNo(), entry);
            journalMapper.insertJournalDetail(detail);

            // 貸借明細登録
            for (JournalLine line : entry.getLines()) {
                JournalDetailItem item = toItemEntity(journal.getJournalNo(), entry.getLineNumber(), line);
                journalMapper.insertJournalDetailItem(item);
            }
        }

        return findByJournalNo(journal.getJournalNo()).orElse(journal);
    }

    @Override
    @Transactional
    public void deleteByJournalNo(String journalNo) {
        journalMapper.deleteByJournalNo(journalNo);
    }

    /**
     * EntityからDomain Modelへ変換
     */
    private Journal toDomainModel(com.example.accounting.infrastructure.out.persistence.entity.Journal entity) {
        if (entity == null) {
            return null;
        }

        Journal domain = new Journal();
        domain.setJournalNo(entity.getJournalNo());
        domain.setJournalDate(entity.getJournalDate());
        domain.setInputDate(entity.getInputDate());
        domain.setSettlementFlag(entity.getSettlementFlag() != null && entity.getSettlementFlag() == 1);
        domain.setSingleEntryFlag(entity.getSingleEntryFlag() != null && entity.getSingleEntryFlag() == 1);
        domain.setJournalType(entity.getJournalType());
        domain.setRecurringFlag(entity.getRecurringFlag() != null && entity.getRecurringFlag() == 1);
        domain.setEmployeeCode(entity.getEmployeeCode());
        domain.setDepartmentCode(entity.getDepartmentCode());
        domain.setRedSlipFlag(entity.getRedSlipFlag() != null && entity.getRedSlipFlag() == 1);
        domain.setRedBlackVoucherNo(entity.getRedBlackVoucherNo());

        // 明細をドメインモデルに変換
        if (entity.getDetails() != null) {
            for (JournalDetail detail : entity.getDetails()) {
                JournalEntry entry = toEntryDomainModel(detail);
                domain.addEntry(entry);
            }
        }

        return domain;
    }

    /**
     * JournalDetailからJournalEntryへ変換
     */
    private JournalEntry toEntryDomainModel(JournalDetail detail) {
        JournalEntry entry = new JournalEntry();
        entry.setLineNumber(detail.getLineNumber());
        entry.setDescription(detail.getDescription());

        // 貸借明細をドメインモデルに変換
        if (detail.getItems() != null) {
            for (JournalDetailItem item : detail.getItems()) {
                JournalLine line = toLineDomainModel(item);
                entry.addLine(line);
            }
        }

        return entry;
    }

    /**
     * JournalDetailItemからJournalLineへ変換
     */
    private JournalLine toLineDomainModel(JournalDetailItem item) {
        JournalLine line = new JournalLine();
        line.setDebitCreditFlag(item.getDebitCreditFlag());
        line.setCurrencyCode(item.getCurrencyCode());
        line.setExchangeRate(item.getExchangeRate());
        line.setDepartmentCode(item.getDepartmentCode());
        line.setProjectCode(item.getProjectCode());
        line.setAccountCode(item.getAccountCode());
        line.setSubAccountCode(item.getSubAccountCode());
        line.setAmount(item.getAmount());
        line.setBaseAmount(item.getBaseAmount());
        line.setTaxType(item.getTaxType());
        line.setTaxRate(item.getTaxRate());
        line.setTaxCalcType(item.getTaxCalcType());
        line.setDueDate(item.getDueDate());
        line.setCashFlowFlag(item.getCashFlowFlag() != null && item.getCashFlowFlag() == 1);
        line.setSegmentCode(item.getSegmentCode());
        line.setOffsetAccountCode(item.getOffsetAccountCode());
        line.setOffsetSubAccountCode(item.getOffsetSubAccountCode());
        line.setNoteCode(item.getNoteCode());
        line.setNoteContent(item.getNoteContent());

        return line;
    }

    /**
     * Domain ModelからEntityへ変換
     */
    private com.example.accounting.infrastructure.out.persistence.entity.Journal toEntity(Journal domain) {
        com.example.accounting.infrastructure.out.persistence.entity.Journal entity =
                new com.example.accounting.infrastructure.out.persistence.entity.Journal();
        entity.setJournalNo(domain.getJournalNo());
        entity.setJournalDate(domain.getJournalDate());
        entity.setInputDate(domain.getInputDate());
        entity.setSettlementFlag(domain.isSettlementFlag() ? 1 : 0);
        entity.setSingleEntryFlag(domain.isSingleEntryFlag() ? 1 : 0);
        entity.setJournalType(domain.getJournalType());
        entity.setRecurringFlag(domain.isRecurringFlag() ? 1 : 0);
        entity.setEmployeeCode(domain.getEmployeeCode());
        entity.setDepartmentCode(domain.getDepartmentCode());
        entity.setRedSlipFlag(domain.isRedSlipFlag() ? 1 : 0);
        entity.setRedBlackVoucherNo(domain.getRedBlackVoucherNo());

        return entity;
    }

    /**
     * JournalEntryからJournalDetailへ変換
     */
    private JournalDetail toDetailEntity(String journalNo, JournalEntry entry) {
        JournalDetail detail = new JournalDetail();
        detail.setJournalNo(journalNo);
        detail.setLineNumber(entry.getLineNumber());
        detail.setDescription(entry.getDescription());

        return detail;
    }

    /**
     * JournalLineからJournalDetailItemへ変換
     */
    private JournalDetailItem toItemEntity(String journalNo, Integer lineNumber, JournalLine line) {
        JournalDetailItem item = new JournalDetailItem();
        item.setJournalNo(journalNo);
        item.setLineNumber(lineNumber);
        item.setDebitCreditFlag(line.getDebitCreditFlag());
        item.setCurrencyCode(line.getCurrencyCode());
        item.setExchangeRate(line.getExchangeRate());
        item.setDepartmentCode(line.getDepartmentCode());
        item.setProjectCode(line.getProjectCode());
        item.setAccountCode(line.getAccountCode());
        item.setSubAccountCode(line.getSubAccountCode());
        item.setAmount(line.getAmount());
        item.setBaseAmount(line.getBaseAmount());
        item.setTaxType(line.getTaxType());
        item.setTaxRate(line.getTaxRate());
        item.setTaxCalcType(line.getTaxCalcType());
        item.setDueDate(line.getDueDate());
        item.setCashFlowFlag(line.isCashFlowFlag() ? 1 : 0);
        item.setSegmentCode(line.getSegmentCode());
        item.setOffsetAccountCode(line.getOffsetAccountCode());
        item.setOffsetSubAccountCode(line.getOffsetSubAccountCode());
        item.setNoteCode(line.getNoteCode());
        item.setNoteContent(line.getNoteContent());

        return item;
    }
}
