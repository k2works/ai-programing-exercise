package com.example.production.infrastructure.persistence.repository;

import com.example.production.application.port.out.BomRepository;
import com.example.production.domain.model.bom.Bom;
import com.example.production.domain.model.bom.BomExplosion;
import com.example.production.infrastructure.persistence.mapper.BomMapper;
import org.springframework.stereotype.Repository;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.List;

@Repository
public class BomRepositoryImpl implements BomRepository {

    private final BomMapper bomMapper;

    public BomRepositoryImpl(BomMapper bomMapper) {
        this.bomMapper = bomMapper;
    }

    @Override
    public void save(Bom bom) {
        bomMapper.insert(bom);
    }

    @Override
    public List<Bom> findByParentItemCode(String parentItemCode) {
        return bomMapper.findByParentItemCode(parentItemCode);
    }

    @Override
    public List<Bom> findByParentItemCodeAndDate(String parentItemCode, LocalDate baseDate) {
        return bomMapper.findByParentItemCodeAndDate(parentItemCode, baseDate);
    }

    @Override
    public List<Bom> findByChildItemCode(String childItemCode) {
        return bomMapper.findByChildItemCode(childItemCode);
    }

    @Override
    public List<BomExplosion> explode(String itemCode, BigDecimal quantity) {
        return bomMapper.explode(itemCode, quantity);
    }

    @Override
    public List<Bom> findAll() {
        return bomMapper.findAll();
    }

    @Override
    public void deleteAll() {
        bomMapper.deleteAll();
    }
}
