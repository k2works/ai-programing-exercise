package com.example.accounting.infrastructure.adapter.out.persistence;

import com.example.accounting.domain.Journal;
import com.example.accounting.domain.JournalEntry;
import com.example.accounting.infrastructure.adapter.out.persistence.mybatis.mapper.JournalMapper;
import com.example.accounting.port.out.JournalRepository;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Optional;

@Repository
public class JournalRepositoryImpl implements JournalRepository {

    private final JournalMapper journalMapper;

    public JournalRepositoryImpl(JournalMapper journalMapper) {
        this.journalMapper = journalMapper;
    }

    @Override
    @Transactional
    public Journal save(Journal journal) {
        // 仕訳ヘッダーを保存
        journalMapper.insertJournal(journal);

        // 仕訳明細を保存
        for (JournalEntry entry : journal.getEntries()) {
            entry.setJournalId(journal.getJournalId());
            journalMapper.insertJournalEntry(entry);
        }

        return journal;
    }

    @Override
    public Optional<Journal> findById(Integer journalId) {
        Journal journal = journalMapper.findById(journalId);
        if (journal != null) {
            List<JournalEntry> entries = journalMapper.findEntriesByJournalId(journalId);
            entries.forEach(journal::addEntry);
        }
        return Optional.ofNullable(journal);
    }

    @Override
    public List<Journal> findByFiscalYear(Integer fiscalYear) {
        return journalMapper.findByFiscalYear(fiscalYear);
    }

    @Override
    public List<JournalEntry> findEntriesByJournalId(Integer journalId) {
        return journalMapper.findEntriesByJournalId(journalId);
    }
}
