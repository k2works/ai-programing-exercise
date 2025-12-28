package com.example.production.infrastructure.out.repository;

import com.example.production.application.port.out.ReceivingRepository;
import com.example.production.domain.model.purchase.Receiving;
import com.example.production.infrastructure.out.mapper.ReceivingMapper;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

/**
 * 入荷受入リポジトリ実装
 */
@Repository
public class ReceivingRepositoryImpl implements ReceivingRepository {

    private final ReceivingMapper receivingMapper;

    public ReceivingRepositoryImpl(ReceivingMapper receivingMapper) {
        this.receivingMapper = receivingMapper;
    }

    @Override
    public void save(Receiving receiving) {
        receivingMapper.insert(receiving);
    }

    @Override
    public Optional<Receiving> findById(Integer id) {
        return receivingMapper.findById(id);
    }

    @Override
    public Optional<Receiving> findByReceivingNumber(String receivingNumber) {
        return receivingMapper.findByReceivingNumber(receivingNumber);
    }

    @Override
    public List<Receiving> findByPurchaseOrderNumber(String purchaseOrderNumber) {
        return receivingMapper.findByPurchaseOrderNumber(purchaseOrderNumber);
    }

    @Override
    public List<Receiving> findAll() {
        return receivingMapper.findAll();
    }

    @Override
    public Optional<String> findLatestReceivingNumber(String prefix) {
        return receivingMapper.findLatestReceivingNumber(prefix);
    }

    @Override
    public void deleteAll() {
        receivingMapper.deleteAll();
    }
}
