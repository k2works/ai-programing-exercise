package com.example.accounting.infrastructure.adapter.in.web;

import com.example.accounting.domain.Journal;
import com.example.accounting.port.in.CreateJournalUseCase;
import com.example.accounting.port.in.GetJournalsUseCase;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@RequestMapping("/api/journals")
public class JournalController {

    private final CreateJournalUseCase createJournalUseCase;
    private final GetJournalsUseCase getJournalsUseCase;

    public JournalController(CreateJournalUseCase createJournalUseCase, GetJournalsUseCase getJournalsUseCase) {
        this.createJournalUseCase = createJournalUseCase;
        this.getJournalsUseCase = getJournalsUseCase;
    }

    @PostMapping
    public ResponseEntity<Journal> createJournal(@RequestBody CreateJournalRequest request) {
        Journal journal = createJournalUseCase.createJournal(
            request.getJournalDate(),
            request.getDescription(),
            request.getFiscalYear(),
            request.getEntries()
        );
        return ResponseEntity.status(HttpStatus.CREATED).body(journal);
    }

    @GetMapping
    public ResponseEntity<List<Journal>> getJournals(@RequestParam Integer fiscalYear) {
        List<Journal> journals = getJournalsUseCase.getJournalsByFiscalYear(fiscalYear);
        return ResponseEntity.ok(journals);
    }
}
