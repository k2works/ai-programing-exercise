package com.example.accounting.application;

import com.example.accounting.domain.Journal;
import com.example.accounting.port.in.GetJournalsUseCase;
import com.example.accounting.port.out.JournalRepository;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

@Service
public class GetJournalsService implements GetJournalsUseCase {

    private final JournalRepository journalRepository;

    public GetJournalsService(JournalRepository journalRepository) {
        this.journalRepository = journalRepository;
    }

    @Override
    @Transactional(readOnly = true)
    public List<Journal> getJournalsByFiscalYear(Integer fiscalYear) {
        return journalRepository.findByFiscalYear(fiscalYear);
    }
}
