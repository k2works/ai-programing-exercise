package com.example.production.application.port.out;

import com.example.production.domain.model.plan.Requirement;

import java.math.BigDecimal;
import java.util.List;
import java.util.Optional;

/**
 * 所要リポジトリ（Output Port）
 */
public interface RequirementRepository {
    void save(Requirement requirement);
    Optional<Requirement> findById(Integer id);
    List<Requirement> findByOrderId(Integer orderId);
    List<Requirement> findAll();
    void updateAllocation(Integer id, BigDecimal allocatedQuantity, BigDecimal shortageQuantity);
    void deleteAll();
}
