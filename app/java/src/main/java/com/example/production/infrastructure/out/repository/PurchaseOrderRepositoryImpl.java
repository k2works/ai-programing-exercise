package com.example.production.infrastructure.out.repository;

import com.example.production.application.port.out.PurchaseOrderRepository;
import com.example.production.domain.model.purchase.PurchaseOrder;
import com.example.production.domain.model.purchase.PurchaseOrderStatus;
import com.example.production.infrastructure.out.mapper.PurchaseOrderMapper;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

/**
 * 発注リポジトリ実装
 */
@Repository
public class PurchaseOrderRepositoryImpl implements PurchaseOrderRepository {

    private final PurchaseOrderMapper purchaseOrderMapper;

    public PurchaseOrderRepositoryImpl(PurchaseOrderMapper purchaseOrderMapper) {
        this.purchaseOrderMapper = purchaseOrderMapper;
    }

    @Override
    public void save(PurchaseOrder purchaseOrder) {
        purchaseOrderMapper.insert(purchaseOrder);
    }

    @Override
    public Optional<PurchaseOrder> findByPurchaseOrderNumber(String purchaseOrderNumber) {
        return purchaseOrderMapper.findByPurchaseOrderNumber(purchaseOrderNumber);
    }

    @Override
    public Optional<String> findLatestPurchaseOrderNumber(String prefix) {
        return purchaseOrderMapper.findLatestPurchaseOrderNumber(prefix);
    }

    @Override
    public void updateStatus(String purchaseOrderNumber, PurchaseOrderStatus status) {
        purchaseOrderMapper.updateStatus(purchaseOrderNumber, status);
    }

    @Override
    public List<PurchaseOrder> findAll() {
        return purchaseOrderMapper.findAll();
    }

    @Override
    public void deleteAll() {
        purchaseOrderMapper.deleteAll();
    }
}
