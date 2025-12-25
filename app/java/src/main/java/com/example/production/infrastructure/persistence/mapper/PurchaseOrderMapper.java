package com.example.production.infrastructure.persistence.mapper;

import com.example.production.domain.model.purchase.PurchaseOrder;
import com.example.production.domain.model.purchase.PurchaseOrderStatus;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.Optional;

@Mapper
public interface PurchaseOrderMapper {
    void insert(PurchaseOrder purchaseOrder);

    Optional<PurchaseOrder> findByPurchaseOrderNumber(String purchaseOrderNumber);

    Optional<String> findLatestPurchaseOrderNumber(String prefix);

    void updateStatus(@Param("purchaseOrderNumber") String purchaseOrderNumber,
                      @Param("status") PurchaseOrderStatus status);

    void deleteAll();
}
