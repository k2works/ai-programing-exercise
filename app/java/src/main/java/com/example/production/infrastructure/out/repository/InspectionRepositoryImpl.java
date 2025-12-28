package com.example.production.infrastructure.out.repository;

import com.example.production.application.port.out.InspectionRepository;
import com.example.production.domain.model.purchase.Inspection;
import com.example.production.infrastructure.out.mapper.InspectionMapper;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

/**
 * 受入検査リポジトリ実装
 */
@Repository
public class InspectionRepositoryImpl implements InspectionRepository {

    private final InspectionMapper inspectionMapper;

    public InspectionRepositoryImpl(InspectionMapper inspectionMapper) {
        this.inspectionMapper = inspectionMapper;
    }

    @Override
    public void save(Inspection inspection) {
        inspectionMapper.insert(inspection);
    }

    @Override
    public Optional<Inspection> findById(Integer id) {
        return inspectionMapper.findById(id);
    }

    @Override
    public Optional<Inspection> findByInspectionNumber(String inspectionNumber) {
        return inspectionMapper.findByInspectionNumber(inspectionNumber);
    }

    @Override
    public List<Inspection> findByReceivingNumber(String receivingNumber) {
        return inspectionMapper.findByReceivingNumber(receivingNumber);
    }

    @Override
    public List<Inspection> findAll() {
        return inspectionMapper.findAll();
    }

    @Override
    public Optional<String> findLatestInspectionNumber(String prefix) {
        return inspectionMapper.findLatestInspectionNumber(prefix);
    }

    @Override
    public void deleteAll() {
        inspectionMapper.deleteAll();
    }
}
