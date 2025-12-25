package com.example.production.infrastructure.persistence.mapper;

import com.example.production.domain.model.process.Routing;
import org.apache.ibatis.annotations.Mapper;

import java.util.List;
import java.util.Optional;

@Mapper
public interface RoutingMapper {
    void insert(Routing routing);
    Optional<Routing> findById(Integer id);
    List<Routing> findByItemCode(String itemCode);
    void deleteAll();
}
