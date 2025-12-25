package com.example.production.infrastructure.persistence.repository;

import com.example.production.application.port.out.PurchaseOrderDetailRepository;
import com.example.production.domain.model.purchase.PurchaseOrderDetail;
import com.example.production.infrastructure.persistence.mapper.PurchaseOrderDetailMapper;
import org.springframework.stereotype.Repository;

import java.math.BigDecimal;
import java.util.List;
import java.util.Optional;

/**
 * 発注明細リポジトリ実装
 */
@Repository
public class PurchaseOrderDetailRepositoryImpl implements PurchaseOrderDetailRepository {

    private final PurchaseOrderDetailMapper detailMapper;

    public PurchaseOrderDetailRepositoryImpl(PurchaseOrderDetailMapper detailMapper) {
        this.detailMapper = detailMapper;
    }

    @Override
    public void save(PurchaseOrderDetail detail) {
        detailMapper.insert(detail);
    }

    @Override
    public Optional<PurchaseOrderDetail> findById(Integer id) {
        return detailMapper.findById(id);
    }

    @Override
    public List<PurchaseOrderDetail> findByPurchaseOrderNumber(String purchaseOrderNumber) {
        return detailMapper.findByPurchaseOrderNumber(purchaseOrderNumber);
    }

    @Override
    public void updateReceivedQuantity(Integer id, BigDecimal receivedQuantity) {
        detailMapper.updateReceivedQuantity(id, receivedQuantity);
    }

    @Override
    public void updateAcceptedQuantity(Integer id, BigDecimal acceptedQuantity) {
        detailMapper.updateAcceptedQuantity(id, acceptedQuantity);
    }

    @Override
    public void updateCompletedFlag(Integer id, Boolean completedFlag) {
        detailMapper.updateCompletedFlag(id, completedFlag);
    }

    @Override
    public void deleteAll() {
        detailMapper.deleteAll();
    }
}
