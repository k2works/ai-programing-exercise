package com.example.production.application.port.out;

import com.example.production.domain.model.process.Routing;

import java.util.List;
import java.util.Optional;

/**
 * 工程表リポジトリ（Output Port）
 */
public interface RoutingRepository {
    void save(Routing routing);
    Optional<Routing> findById(Integer id);
    List<Routing> findByItemCode(String itemCode);
    List<Routing> findAll();
    void deleteAll();
}
