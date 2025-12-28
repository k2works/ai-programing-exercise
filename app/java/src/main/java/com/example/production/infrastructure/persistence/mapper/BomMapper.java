package com.example.production.infrastructure.persistence.mapper;

import com.example.production.domain.model.bom.Bom;
import com.example.production.domain.model.bom.BomExplosion;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.List;

@Mapper
public interface BomMapper {
    void insert(Bom bom);

    void update(Bom bom);

    void delete(@Param("parentItemCode") String parentItemCode,
                @Param("childItemCode") String childItemCode,
                @Param("effectiveFrom") LocalDate effectiveFrom);

    Bom findByKey(@Param("parentItemCode") String parentItemCode,
                  @Param("childItemCode") String childItemCode,
                  @Param("effectiveFrom") LocalDate effectiveFrom);

    List<Bom> findByParentItemCode(String parentItemCode);

    List<Bom> findByParentItemCodeAndDate(@Param("parentItemCode") String parentItemCode,
                                           @Param("baseDate") LocalDate baseDate);

    List<Bom> findByChildItemCode(String childItemCode);

    List<BomExplosion> explode(@Param("itemCode") String itemCode,
                               @Param("quantity") BigDecimal quantity);

    List<Bom> findAll();

    void deleteAll();
}
