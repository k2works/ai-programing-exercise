package com.example.accounting.infrastructure.web.controller;

import com.example.accounting.application.service.FinancialStatementService;
import com.example.accounting.application.model.financial.BalanceSheet;
import com.example.accounting.application.model.financial.FinancialRatios;
import com.example.accounting.application.model.financial.IncomeStatement;
import com.example.accounting.infrastructure.web.dto.BalanceSheetResponse;
import com.example.accounting.infrastructure.web.dto.FinancialRatiosResponse;
import com.example.accounting.infrastructure.web.dto.IncomeStatementResponse;
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
@RestController
@RequestMapping("/api/v1/financial-statements")
public class FinancialStatementController {

    private final FinancialStatementService financialStatementService;

    public FinancialStatementController(FinancialStatementService financialStatementService) {
        this.financialStatementService = financialStatementService;
    }

    /**
     * 貸借対照表を取得
     *
     * @param asOfDate 基準日（デフォルト：システム日付）
     * @return 貸借対照表
     */
    @GetMapping("/balance-sheet")
    public ResponseEntity<BalanceSheetResponse> getBalanceSheet(
            @RequestParam(required = false)
            @DateTimeFormat(iso = DateTimeFormat.ISO.DATE)
            LocalDate asOfDate) {

        LocalDate targetDate = asOfDate != null ? asOfDate : LocalDate.now();
        BalanceSheet balanceSheet = financialStatementService.generateBalanceSheet(targetDate);
        return ResponseEntity.ok(BalanceSheetResponse.from(balanceSheet));
    }

    /**
     * 損益計算書を取得
     *
     * @param fromDate 開始日（デフォルト：当月初日）
     * @param toDate 終了日（デフォルト：システム日付）
     * @return 損益計算書
     */
    @GetMapping("/income-statement")
    public ResponseEntity<IncomeStatementResponse> getIncomeStatement(
            @RequestParam(required = false)
            @DateTimeFormat(iso = DateTimeFormat.ISO.DATE)
            LocalDate fromDate,
            @RequestParam(required = false)
            @DateTimeFormat(iso = DateTimeFormat.ISO.DATE)
            LocalDate toDate) {

        LocalDate from = fromDate != null ? fromDate : LocalDate.now().withDayOfMonth(1);
        LocalDate to = toDate != null ? toDate : LocalDate.now();

        IncomeStatement incomeStatement = financialStatementService.generateIncomeStatement(from, to);
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
    @GetMapping("/financial-ratios")
    public ResponseEntity<FinancialRatiosResponse> getFinancialRatios(
            @RequestParam(required = false)
            @DateTimeFormat(iso = DateTimeFormat.ISO.DATE)
            LocalDate asOfDate,
            @RequestParam(required = false)
            @DateTimeFormat(iso = DateTimeFormat.ISO.DATE)
            LocalDate fromDate,
            @RequestParam(required = false)
            @DateTimeFormat(iso = DateTimeFormat.ISO.DATE)
            LocalDate toDate) {

        LocalDate bsDate = asOfDate != null ? asOfDate : LocalDate.now();
        LocalDate plFrom = fromDate != null ? fromDate : LocalDate.now().withDayOfMonth(1);
        LocalDate plTo = toDate != null ? toDate : LocalDate.now();

        BalanceSheet balanceSheet = financialStatementService.generateBalanceSheet(bsDate);
        IncomeStatement incomeStatement = financialStatementService.generateIncomeStatement(plFrom, plTo);
        FinancialRatios ratios = financialStatementService.calculateFinancialRatios(
                balanceSheet, incomeStatement);

        return ResponseEntity.ok(FinancialRatiosResponse.from(ratios));
    }
}
