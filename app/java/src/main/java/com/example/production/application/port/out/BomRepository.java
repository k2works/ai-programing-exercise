package com.example.production.application.port.out;

import com.example.production.domain.model.bom.Bom;
import com.example.production.domain.model.bom.BomExplosion;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.List;

/**
 * BOMリポジトリ（Output Port）
 */
public interface BomRepository {

    void save(Bom bom);

    List<Bom> findByParentItemCode(String parentItemCode);

    List<Bom> findByParentItemCodeAndDate(String parentItemCode, LocalDate baseDate);

    List<Bom> findByChildItemCode(String childItemCode);

    List<BomExplosion> explode(String itemCode, BigDecimal quantity);

    void deleteAll();
}
