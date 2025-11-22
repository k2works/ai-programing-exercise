package com.example.accounting.infrastructure.persistence.repository;

import com.example.accounting.application.port.out.FinancialDataRepository;
import com.example.accounting.domain.model.financial.BalanceSheetItem;
import com.example.accounting.domain.model.financial.IncomeStatementItem;
import com.example.accounting.infrastructure.persistence.mapper.FinancialDataMapper;
import org.springframework.stereotype.Repository;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * 財務データリポジトリ実装（Output Adapter）
 */
@Repository
public class FinancialDataRepositoryImpl implements FinancialDataRepository {

    private final FinancialDataMapper mapper;

    public FinancialDataRepositoryImpl(FinancialDataMapper mapper) {
        this.mapper = mapper;
    }

    @Override
    public List<BalanceSheetItem> findBalanceSheetItems(LocalDate asOfDate, String elementType) {
        List<Map<String, Object>> results = mapper.findBalanceSheetItems(asOfDate, elementType);

        return results.stream()
                .map(this::mapToBalanceSheetItem)
                .collect(Collectors.toList());
    }

    @Override
    public List<IncomeStatementItem> findIncomeStatementItems(
            LocalDate fromDate, LocalDate toDate, String elementType) {
        List<Map<String, Object>> results = mapper.findIncomeStatementItems(
                fromDate, toDate, elementType);

        return results.stream()
                .map(this::mapToIncomeStatementItem)
                .collect(Collectors.toList());
    }

    /**
     * MapからBalanceSheetItemへ変換
     */
    private BalanceSheetItem mapToBalanceSheetItem(Map<String, Object> row) {
        String accountCode = (String) row.get("accountcode");
        String accountName = (String) row.get("accountname");
        BigDecimal balance = (BigDecimal) row.get("balance");

        return new BalanceSheetItem(
                accountCode,
                accountName,
                balance,
                BigDecimal.ZERO  // ratioは後で計算される
        );
    }

    /**
     * MapからIncomeStatementItemへ変換
     */
    private IncomeStatementItem mapToIncomeStatementItem(Map<String, Object> row) {
        String accountCode = (String) row.get("accountcode");
        String accountName = (String) row.get("accountname");
        BigDecimal balance = (BigDecimal) row.get("balance");

        return new IncomeStatementItem(
                accountCode,
                accountName,
                balance,
                BigDecimal.ZERO  // percentageは後で計算される
        );
    }
}
