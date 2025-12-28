package com.example.production.infrastructure.out.repository;

import com.example.production.application.port.out.OrderRepository;
import com.example.production.domain.model.plan.Order;
import com.example.production.domain.model.plan.PlanStatus;
import com.example.production.infrastructure.out.mapper.OrderMapper;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

/**
 * オーダリポジトリ実装
 */
@Repository
public class OrderRepositoryImpl implements OrderRepository {

    private final OrderMapper orderMapper;

    public OrderRepositoryImpl(OrderMapper orderMapper) {
        this.orderMapper = orderMapper;
    }

    @Override
    public void save(Order order) {
        orderMapper.insert(order);
    }

    @Override
    public Optional<Order> findById(Integer id) {
        return orderMapper.findById(id);
    }

    @Override
    public Optional<Order> findByOrderNumber(String orderNumber) {
        return orderMapper.findByOrderNumber(orderNumber);
    }

    @Override
    public List<Order> findByMpsId(Integer mpsId) {
        return orderMapper.findByMpsId(mpsId);
    }

    @Override
    public List<Order> findByParentOrderId(Integer parentOrderId) {
        return orderMapper.findByParentOrderId(parentOrderId);
    }

    @Override
    public void updateParentOrderId(Integer id, Integer parentOrderId) {
        orderMapper.updateParentOrderId(id, parentOrderId);
    }

    @Override
    public void updateStatus(Integer id, PlanStatus status) {
        orderMapper.updateStatus(id, status);
    }

    @Override
    public List<Order> findAll() {
        return orderMapper.findAll();
    }

    @Override
    public void deleteAll() {
        orderMapper.deleteAll();
    }
}
