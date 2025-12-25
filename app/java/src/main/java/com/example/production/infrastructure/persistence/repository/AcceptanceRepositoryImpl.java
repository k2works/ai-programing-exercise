package com.example.production.infrastructure.persistence.repository;

import com.example.production.application.port.out.AcceptanceRepository;
import com.example.production.domain.model.purchase.Acceptance;
import com.example.production.infrastructure.persistence.mapper.AcceptanceMapper;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

/**
 * 検収リポジトリ実装
 */
@Repository
public class AcceptanceRepositoryImpl implements AcceptanceRepository {

    private final AcceptanceMapper acceptanceMapper;

    public AcceptanceRepositoryImpl(AcceptanceMapper acceptanceMapper) {
        this.acceptanceMapper = acceptanceMapper;
    }

    @Override
    public void save(Acceptance acceptance) {
        acceptanceMapper.insert(acceptance);
    }

    @Override
    public Optional<Acceptance> findById(Integer id) {
        return acceptanceMapper.findById(id);
    }

    @Override
    public Optional<Acceptance> findByAcceptanceNumber(String acceptanceNumber) {
        return acceptanceMapper.findByAcceptanceNumber(acceptanceNumber);
    }

    @Override
    public List<Acceptance> findByInspectionNumber(String inspectionNumber) {
        return acceptanceMapper.findByInspectionNumber(inspectionNumber);
    }

    @Override
    public List<Acceptance> findByPurchaseOrderNumber(String purchaseOrderNumber) {
        return acceptanceMapper.findByPurchaseOrderNumber(purchaseOrderNumber);
    }

    @Override
    public Optional<String> findLatestAcceptanceNumber(String prefix) {
        return acceptanceMapper.findLatestAcceptanceNumber(prefix);
    }

    @Override
    public void deleteAll() {
        acceptanceMapper.deleteAll();
    }
}
