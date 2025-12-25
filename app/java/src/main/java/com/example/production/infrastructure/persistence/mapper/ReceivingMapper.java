package com.example.production.infrastructure.persistence.mapper;

import com.example.production.domain.model.purchase.Receiving;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;
import java.util.Optional;

@Mapper
public interface ReceivingMapper {

    void insert(Receiving receiving);

    Optional<Receiving> findById(@Param("id") Integer id);

    Optional<Receiving> findByReceivingNumber(@Param("receivingNumber") String receivingNumber);

    List<Receiving> findByPurchaseOrderNumber(@Param("purchaseOrderNumber") String purchaseOrderNumber);

    Optional<String> findLatestReceivingNumber(@Param("prefix") String prefix);

    void deleteAll();
}
