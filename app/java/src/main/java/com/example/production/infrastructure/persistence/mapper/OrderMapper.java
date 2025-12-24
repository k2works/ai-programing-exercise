package com.example.production.infrastructure.persistence.mapper;

import com.example.production.domain.model.plan.Order;
import com.example.production.domain.model.plan.PlanStatus;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;
import java.util.Optional;

@Mapper
public interface OrderMapper {
    void insert(Order order);

    Optional<Order> findById(Integer id);

    Optional<Order> findByOrderNumber(String orderNumber);

    List<Order> findByMpsId(Integer mpsId);

    List<Order> findByParentOrderId(Integer parentOrderId);

    void updateParentOrderId(@Param("id") Integer id, @Param("parentOrderId") Integer parentOrderId);

    void updateStatus(@Param("id") Integer id, @Param("status") PlanStatus status);

    void deleteAll();
}
