package com.example.accounting.port.out;

import com.example.accounting.domain.Journal;
import com.example.accounting.domain.JournalEntry;
import java.util.List;
import java.util.Optional;

public interface JournalRepository {
    Journal save(Journal journal);
    Optional<Journal> findById(Integer journalId);
    List<Journal> findByFiscalYear(Integer fiscalYear);
    List<JournalEntry> findEntriesByJournalId(Integer journalId);
}
