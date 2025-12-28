package com.example.production.infrastructure.out.mapper;

import com.example.production.domain.model.purchase.PurchaseOrderDetail;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.math.BigDecimal;
import java.util.List;
import java.util.Optional;

@Mapper
public interface PurchaseOrderDetailMapper {
    void insert(PurchaseOrderDetail detail);

    Optional<PurchaseOrderDetail> findById(Integer id);

    List<PurchaseOrderDetail> findByPurchaseOrderNumber(String purchaseOrderNumber);

    void updateReceivedQuantity(@Param("id") Integer id, @Param("receivedQuantity") BigDecimal receivedQuantity);

    void updateAcceptedQuantity(@Param("id") Integer id, @Param("acceptedQuantity") BigDecimal acceptedQuantity);

    void updateCompletedFlag(@Param("id") Integer id, @Param("completedFlag") Boolean completedFlag);

    void deleteAll();
}
