package com.example.production.application.port.out;

import com.example.production.domain.model.plan.Order;
import com.example.production.domain.model.plan.PlanStatus;

import java.util.List;
import java.util.Optional;

/**
 * オーダリポジトリ（Output Port）
 */
public interface OrderRepository {
    void save(Order order);
    Optional<Order> findById(Integer id);
    Optional<Order> findByOrderNumber(String orderNumber);
    List<Order> findByMpsId(Integer mpsId);
    List<Order> findByParentOrderId(Integer parentOrderId);
    void updateParentOrderId(Integer id, Integer parentOrderId);
    void updateStatus(Integer id, PlanStatus status);
    List<Order> findAll();
    void deleteAll();
}
