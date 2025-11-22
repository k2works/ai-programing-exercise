package com.example.accounting.infrastructure.adapter.in.web;

import com.example.accounting.domain.Journal;
import com.example.accounting.port.in.CreateJournalUseCase;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/api/journals")
public class JournalController {

    private final CreateJournalUseCase createJournalUseCase;

    public JournalController(CreateJournalUseCase createJournalUseCase) {
        this.createJournalUseCase = createJournalUseCase;
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
}
