package com.example.production.application.port.out;

import com.example.production.domain.model.purchase.Receiving;

import java.util.List;
import java.util.Optional;

/**
 * 入荷受入リポジトリ
 */
public interface ReceivingRepository {

    void save(Receiving receiving);

    Optional<Receiving> findById(Integer id);

    Optional<Receiving> findByReceivingNumber(String receivingNumber);

    List<Receiving> findByPurchaseOrderNumber(String purchaseOrderNumber);

    Optional<String> findLatestReceivingNumber(String prefix);

    void deleteAll();
}
