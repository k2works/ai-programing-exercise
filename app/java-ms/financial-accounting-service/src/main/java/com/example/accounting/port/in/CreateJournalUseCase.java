package com.example.accounting.port.in;

import com.example.accounting.domain.Journal;
import java.time.LocalDate;
import java.util.List;

public interface CreateJournalUseCase {
    Journal createJournal(LocalDate journalDate, String description, Integer fiscalYear, List<JournalEntryRequest> entries);
}
