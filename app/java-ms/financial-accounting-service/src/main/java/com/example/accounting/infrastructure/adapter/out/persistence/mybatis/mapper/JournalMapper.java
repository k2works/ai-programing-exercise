package com.example.accounting.infrastructure.adapter.out.persistence.mybatis.mapper;

import com.example.accounting.domain.Journal;
import com.example.accounting.domain.JournalEntry;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

@Mapper
public interface JournalMapper {
    void insertJournal(Journal journal);
    void insertJournalEntry(JournalEntry entry);
    Journal findById(@Param("journalId") Integer journalId);
    List<Journal> findByFiscalYear(@Param("fiscalYear") Integer fiscalYear);
    List<JournalEntry> findEntriesByJournalId(@Param("journalId") Integer journalId);
}
