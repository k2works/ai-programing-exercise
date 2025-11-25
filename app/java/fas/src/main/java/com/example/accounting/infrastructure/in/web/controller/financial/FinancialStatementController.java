package com.example.accounting.infrastructure.in.web.controller.financial;

import com.example.accounting.application.port.in.FinancialStatementUseCase;
import com.example.accounting.domain.model.financial.BalanceSheet;
import com.example.accounting.domain.model.financial.FinancialRatios;
import com.example.accounting.domain.model.financial.IncomeStatement;
import com.example.accounting.infrastructure.in.web.dto.financial.BalanceSheetResponse;
import com.example.accounting.infrastructure.in.web.dto.financial.FinancialRatiosResponse;
import com.example.accounting.infrastructure.in.web.dto.financial.IncomeStatementResponse;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import java.time.LocalDate;

/**
 * 財務諸表 REST API コントローラー（Input Adapter）
 */
@Tag(name = "財務諸表", description = "財務諸表・財務指標生成API")
@RestController
@RequestMapping("/api/v1/financial-statements")
public class FinancialStatementController {

    private final FinancialStatementUseCase financialStatementUseCase;

    public FinancialStatementController(FinancialStatementUseCase financialStatementUseCase) {
        this.financialStatementUseCase = financialStatementUseCase;
    }

    /**
     * 貸借対照表を取得
     *
     * @param asOfDate 基準日（デフォルト：システム日付）
     * @return 貸借対照表
     */
    @Operation(summary = "貸借対照表取得", description = "指定日時点の貸借対照表を生成します")
    @ApiResponses(value = {
        @ApiResponse(responseCode = "200", description = "正常に取得",
                content = @Content(schema = @Schema(implementation = BalanceSheetResponse.class)))
    })
    @GetMapping("/balance-sheet")
    public ResponseEntity<BalanceSheetResponse> getBalanceSheet(
            @Parameter(description = "基準日（省略時は本日）", example = "2025-03-31")
            @RequestParam(required = false)
            @DateTimeFormat(iso = DateTimeFormat.ISO.DATE)
            LocalDate asOfDate) {

        LocalDate targetDate = asOfDate != null ? asOfDate : LocalDate.now();
        BalanceSheet balanceSheet = financialStatementUseCase.generateBalanceSheet(targetDate);
        return ResponseEntity.ok(BalanceSheetResponse.from(balanceSheet));
    }

    /**
     * 損益計算書を取得
     *
     * @param fromDate 開始日（デフォルト：当月初日）
     * @param toDate 終了日（デフォルト：システム日付）
     * @return 損益計算書
     */
    @Operation(summary = "損益計算書取得", description = "指定期間の損益計算書を生成します")
    @ApiResponses(value = {
        @ApiResponse(responseCode = "200", description = "正常に取得",
                content = @Content(schema = @Schema(implementation = IncomeStatementResponse.class)))
    })
    @GetMapping("/income-statement")
    public ResponseEntity<IncomeStatementResponse> getIncomeStatement(
            @Parameter(description = "開始日（省略時は当月1日）", example = "2025-01-01")
            @RequestParam(required = false)
            @DateTimeFormat(iso = DateTimeFormat.ISO.DATE)
            LocalDate fromDate,
            @Parameter(description = "終了日（省略時は本日）", example = "2025-03-31")
            @RequestParam(required = false)
            @DateTimeFormat(iso = DateTimeFormat.ISO.DATE)
            LocalDate toDate) {

        LocalDate from = fromDate != null ? fromDate : LocalDate.now().withDayOfMonth(1);
        LocalDate to = toDate != null ? toDate : LocalDate.now();

        IncomeStatement incomeStatement = financialStatementUseCase.generateIncomeStatement(from, to);
        return ResponseEntity.ok(IncomeStatementResponse.from(incomeStatement));
    }

    /**
     * 財務指標を取得
     *
     * @param asOfDate 貸借対照表の基準日（デフォルト：システム日付）
     * @param fromDate 損益計算書の開始日（デフォルト：当月初日）
     * @param toDate 損益計算書の終了日（デフォルト：システム日付）
     * @return 財務指標
     */
    @Operation(summary = "財務指標取得", description = "貸借対照表と損益計算書から財務指標を計算します")
    @ApiResponses(value = {
        @ApiResponse(responseCode = "200", description = "正常に取得",
                content = @Content(schema = @Schema(implementation = FinancialRatiosResponse.class)))
    })
    @GetMapping("/financial-ratios")
    public ResponseEntity<FinancialRatiosResponse> getFinancialRatios(
            @Parameter(description = "貸借対照表基準日（省略時は本日）", example = "2025-03-31")
            @RequestParam(required = false)
            @DateTimeFormat(iso = DateTimeFormat.ISO.DATE)
            LocalDate asOfDate,
            @Parameter(description = "損益計算書開始日（省略時は当月1日）", example = "2025-01-01")
            @RequestParam(required = false)
            @DateTimeFormat(iso = DateTimeFormat.ISO.DATE)
            LocalDate fromDate,
            @Parameter(description = "損益計算書終了日（省略時は本日）", example = "2025-03-31")
            @RequestParam(required = false)
            @DateTimeFormat(iso = DateTimeFormat.ISO.DATE)
            LocalDate toDate) {

        LocalDate bsDate = asOfDate != null ? asOfDate : LocalDate.now();
        LocalDate plFrom = fromDate != null ? fromDate : LocalDate.now().withDayOfMonth(1);
        LocalDate plTo = toDate != null ? toDate : LocalDate.now();

        BalanceSheet balanceSheet = financialStatementUseCase.generateBalanceSheet(bsDate);
        IncomeStatement incomeStatement = financialStatementUseCase.generateIncomeStatement(plFrom, plTo);
        FinancialRatios ratios = financialStatementUseCase.calculateFinancialRatios(
                balanceSheet, incomeStatement);

        return ResponseEntity.ok(FinancialRatiosResponse.from(ratios));
    }
}
