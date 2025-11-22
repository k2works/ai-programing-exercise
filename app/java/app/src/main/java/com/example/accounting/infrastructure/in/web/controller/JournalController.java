package com.example.accounting.infrastructure.in.web.controller;

import com.example.accounting.application.exception.JournalNotFoundException;
import com.example.accounting.domain.model.Journal;
import com.example.accounting.domain.model.JournalEntry;
import com.example.accounting.domain.model.JournalLine;
import com.example.accounting.application.port.in.JournalUseCase;
import com.example.accounting.infrastructure.in.web.dto.JournalRequest;
import com.example.accounting.infrastructure.in.web.dto.JournalResponse;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.math.BigDecimal;
import java.util.List;
import java.util.stream.Collectors;

/**
 * 仕訳 REST APIコントローラ
 */
@Tag(name = "仕訳", description = "仕訳管理API")
@RestController
@RequestMapping("/api/v1/journals")
public class JournalController {

    private final JournalUseCase journalUseCase;

    public JournalController(JournalUseCase journalUseCase) {
        this.journalUseCase = journalUseCase;
    }

    @Operation(summary = "仕訳一覧取得", description = "登録されているすべての仕訳を取得します")
    @ApiResponses(value = {
        @ApiResponse(responseCode = "200", description = "正常に取得",
                content = @Content(schema = @Schema(implementation = JournalResponse.class)))
    })
    @GetMapping
    public ResponseEntity<List<JournalResponse>> getAllJournals() {
        List<Journal> journals = journalUseCase.getAllJournals();
        List<JournalResponse> response = journals.stream()
                .map(JournalResponse::from)
                .collect(Collectors.toList());
        return ResponseEntity.ok(response);
    }

    @Operation(summary = "仕訳取得", description = "仕訳番号を指定して仕訳を取得します")
    @ApiResponses(value = {
        @ApiResponse(responseCode = "200", description = "正常に取得"),
        @ApiResponse(responseCode = "404", description = "仕訳が見つかりません")
    })
    @GetMapping("/{journalNo}")
    public ResponseEntity<JournalResponse> getJournal(
            @Parameter(description = "仕訳番号", required = true)
            @PathVariable String journalNo) {
        try {
            Journal journal = journalUseCase.getJournalByNo(journalNo);
            return ResponseEntity.ok(JournalResponse.from(journal));
        } catch (JournalNotFoundException e) {
            return ResponseEntity.notFound().build();
        }
    }

    @Operation(summary = "仕訳作成", description = "新しい仕訳を作成します")
    @ApiResponses(value = {
        @ApiResponse(responseCode = "201", description = "正常に作成"),
        @ApiResponse(responseCode = "400", description = "リクエストが不正です")
    })
    @PostMapping
    public ResponseEntity<JournalResponse> createJournal(
            @Parameter(description = "仕訳情報", required = true)
            @RequestBody JournalRequest request) {
        Journal journal = toModel(request);
        Journal created = journalUseCase.createJournal(journal);
        return ResponseEntity.status(HttpStatus.CREATED)
                .body(JournalResponse.from(created));
    }

    @Operation(summary = "仕訳更新", description = "既存の仕訳を更新します")
    @ApiResponses(value = {
        @ApiResponse(responseCode = "200", description = "正常に更新"),
        @ApiResponse(responseCode = "400", description = "リクエストが不正です"),
        @ApiResponse(responseCode = "404", description = "仕訳が見つかりません")
    })
    @PutMapping("/{journalNo}")
    public ResponseEntity<JournalResponse> updateJournal(
            @Parameter(description = "仕訳番号", required = true)
            @PathVariable String journalNo,
            @Parameter(description = "仕訳情報", required = true)
            @RequestBody JournalRequest request) {
        try {
            Journal journal = toModel(request);
            Journal updated = journalUseCase.updateJournal(journalNo, journal);
            return ResponseEntity.ok(JournalResponse.from(updated));
        } catch (JournalNotFoundException e) {
            return ResponseEntity.notFound().build();
        }
    }

    @Operation(summary = "仕訳削除", description = "仕訳を削除します")
    @ApiResponses(value = {
        @ApiResponse(responseCode = "204", description = "正常に削除"),
        @ApiResponse(responseCode = "404", description = "仕訳が見つかりません")
    })
    @DeleteMapping("/{journalNo}")
    public ResponseEntity<Void> deleteJournal(
            @Parameter(description = "仕訳番号", required = true)
            @PathVariable String journalNo) {
        try {
            journalUseCase.deleteJournal(journalNo);
            return ResponseEntity.noContent().build();
        } catch (JournalNotFoundException e) {
            return ResponseEntity.notFound().build();
        }
    }

    /**
     * リクエストからドメインモデルへ変換
     */
    private Journal toModel(JournalRequest request) {
        Journal journal = new Journal();
        journal.setJournalNo(request.journalNo());
        journal.setJournalDate(request.journalDate());
        journal.setInputDate(request.inputDate());
        journal.setSettlementFlag(request.settlementFlag());
        journal.setSingleEntryFlag(request.singleEntryFlag());
        journal.setJournalType(request.journalType());
        journal.setRecurringFlag(request.recurringFlag());
        journal.setEmployeeCode(request.employeeCode());
        journal.setDepartmentCode(request.departmentCode());
        journal.setRedSlipFlag(request.redSlipFlag());
        journal.setRedBlackVoucherNo(request.redBlackVoucherNo());

        if (request.entries() != null) {
            for (JournalRequest.JournalEntryDto entryDto : request.entries()) {
                JournalEntry entry = new JournalEntry();
                entry.setLineNumber(entryDto.lineNumber());
                entry.setDescription(entryDto.description());

                if (entryDto.lines() != null) {
                    for (JournalRequest.JournalLineDto lineDto : entryDto.lines()) {
                        JournalLine line = new JournalLine();
                        line.setDebitCreditFlag(lineDto.debitCreditFlag());
                        line.setCurrencyCode(lineDto.currencyCode());
                        line.setExchangeRate(lineDto.exchangeRate() != null
                                ? new BigDecimal(lineDto.exchangeRate()) : BigDecimal.ONE);
                        line.setDepartmentCode(lineDto.departmentCode());
                        line.setProjectCode(lineDto.projectCode());
                        line.setAccountCode(lineDto.accountCode());
                        line.setSubAccountCode(lineDto.subAccountCode());
                        line.setAmount(new BigDecimal(lineDto.amount()));
                        line.setBaseAmount(lineDto.baseAmount() != null
                                ? new BigDecimal(lineDto.baseAmount())
                                : new BigDecimal(lineDto.amount()));
                        line.setTaxType(lineDto.taxType());
                        line.setTaxRate(lineDto.taxRate());
                        line.setTaxCalcType(lineDto.taxCalcType());
                        line.setDueDate(lineDto.dueDate());
                        line.setCashFlowFlag(lineDto.cashFlowFlag());
                        line.setSegmentCode(lineDto.segmentCode());
                        line.setOffsetAccountCode(lineDto.offsetAccountCode());
                        line.setOffsetSubAccountCode(lineDto.offsetSubAccountCode());
                        line.setNoteCode(lineDto.noteCode());
                        line.setNoteContent(lineDto.noteContent());

                        entry.addLine(line);
                    }
                }

                journal.addEntry(entry);
            }
        }

        return journal;
    }
}
