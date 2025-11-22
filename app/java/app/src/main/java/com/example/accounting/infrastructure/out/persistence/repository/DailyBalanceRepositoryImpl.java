package com.example.accounting.infrastructure.out.persistence.repository;

import com.example.accounting.application.port.out.DailyBalanceRepository;
import com.example.accounting.domain.model.DailyBalance;
import com.example.accounting.infrastructure.out.persistence.entity.DailyAccountBalance;
import com.example.accounting.infrastructure.out.persistence.mapper.DailyAccountBalanceMapper;
import org.springframework.stereotype.Repository;

import java.time.LocalDate;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

/**
 * 日次残高リポジトリ実装（Output Adapter）
 */
@Repository
public class DailyBalanceRepositoryImpl implements DailyBalanceRepository {

    private final DailyAccountBalanceMapper mapper;

    public DailyBalanceRepositoryImpl(DailyAccountBalanceMapper mapper) {
        this.mapper = mapper;
    }

    @Override
    public void saveOrUpdate(DailyBalance balance) {
        DailyAccountBalance entity = toEntity(balance);
        mapper.upsert(entity);
    }

    @Override
    public Optional<DailyBalance> findByKey(
            LocalDate entryDate,
            String accountCode,
            String subAccountCode,
            String departmentCode,
            String projectCode,
            boolean isSettlement) {

        DailyAccountBalance entity = mapper.findByKey(
                entryDate,
                accountCode,
                subAccountCode != null ? subAccountCode : "",
                departmentCode != null ? departmentCode : "",
                projectCode != null ? projectCode : "",
                isSettlement ? 1 : 0
        );

        return Optional.ofNullable(entity).map(this::toDomainModel);
    }

    @Override
    public List<DailyBalance> findByDate(LocalDate entryDate) {
        return mapper.findByDate(entryDate).stream()
                .map(this::toDomainModel)
                .collect(Collectors.toList());
    }

    @Override
    public List<DailyBalance> findByDateRange(LocalDate startDate, LocalDate endDate) {
        return mapper.findByDateRange(startDate, endDate).stream()
                .map(this::toDomainModel)
                .collect(Collectors.toList());
    }

    @Override
    public List<DailyBalance> findByAccountCode(String accountCode) {
        return mapper.findByAccountCode(accountCode).stream()
                .map(this::toDomainModel)
                .collect(Collectors.toList());
    }

    /**
     * EntityからDomain Modelへ変換
     */
    private DailyBalance toDomainModel(DailyAccountBalance entity) {
        if (entity == null) {
            return null;
        }

        DailyBalance domain = new DailyBalance();
        domain.setEntryDate(entity.getEntryDate());
        domain.setAccountCode(entity.getAccountCode());
        domain.setSubAccountCode(entity.getSubAccountCode());
        domain.setDepartmentCode(entity.getDepartmentCode());
        domain.setProjectCode(entity.getProjectCode());
        domain.setSettlement(entity.getSettlementFlag() != null && entity.getSettlementFlag() == 1);
        domain.setDebitAmount(entity.getDebitAmount());
        domain.setCreditAmount(entity.getCreditAmount());

        return domain;
    }

    /**
     * Domain ModelからEntityへ変換
     */
    private DailyAccountBalance toEntity(DailyBalance domain) {
        if (domain == null) {
            return null;
        }

        DailyAccountBalance entity = new DailyAccountBalance();
        entity.setEntryDate(domain.getEntryDate());
        entity.setAccountCode(domain.getAccountCode());
        entity.setSubAccountCode(domain.getSubAccountCode() != null ? domain.getSubAccountCode() : "");
        entity.setDepartmentCode(domain.getDepartmentCode() != null ? domain.getDepartmentCode() : "");
        entity.setProjectCode(domain.getProjectCode() != null ? domain.getProjectCode() : "");
        entity.setSettlementFlag(domain.isSettlement() ? 1 : 0);
        entity.setDebitAmount(domain.getDebitAmount());
        entity.setCreditAmount(domain.getCreditAmount());

        return entity;
    }
}
