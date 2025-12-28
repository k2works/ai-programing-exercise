package com.example.production.infrastructure.out.mapper;

import com.example.production.domain.model.plan.Requirement;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.math.BigDecimal;
import java.util.List;
import java.util.Optional;

@Mapper
public interface RequirementMapper {
    void insert(Requirement requirement);

    Optional<Requirement> findById(Integer id);

    List<Requirement> findByOrderId(Integer orderId);

    List<Requirement> findAll();

    void updateAllocation(@Param("id") Integer id,
                          @Param("allocatedQuantity") BigDecimal allocatedQuantity,
                          @Param("shortageQuantity") BigDecimal shortageQuantity);

    void deleteAll();
}
