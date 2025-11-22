package com.example.accounting.infrastructure.out.persistence.mapper;

import com.example.accounting.infrastructure.out.persistence.entity.TaxTransaction;
import org.apache.ibatis.annotations.Param;

import java.math.BigDecimal;
import java.util.List;

public interface TaxTransactionMapper {
    void insert(TaxTransaction taxTransaction);
    TaxTransaction findByCode(@Param("taxCode") String taxCode);
    List<TaxTransaction> findAll();
    List<TaxTransaction> search(
            @Param("taxName") String taxName,
            @Param("minTaxRate") BigDecimal minTaxRate,
            @Param("isActive") Boolean isActive
    );
    void update(TaxTransaction taxTransaction);
    void delete(@Param("taxCode") String taxCode);
}
