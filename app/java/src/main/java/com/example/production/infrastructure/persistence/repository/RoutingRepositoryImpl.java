package com.example.production.infrastructure.persistence.repository;

import com.example.production.application.port.out.RoutingRepository;
import com.example.production.domain.model.process.Routing;
import com.example.production.infrastructure.persistence.mapper.RoutingMapper;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

/**
 * 工程表リポジトリ実装
 */
@Repository
public class RoutingRepositoryImpl implements RoutingRepository {

    private final RoutingMapper routingMapper;

    public RoutingRepositoryImpl(RoutingMapper routingMapper) {
        this.routingMapper = routingMapper;
    }

    @Override
    public void save(Routing routing) {
        routingMapper.insert(routing);
    }

    @Override
    public Optional<Routing> findById(Integer id) {
        return routingMapper.findById(id);
    }

    @Override
    public List<Routing> findByItemCode(String itemCode) {
        return routingMapper.findByItemCode(itemCode);
    }

    @Override
    public List<Routing> findAll() {
        return routingMapper.findAll();
    }

    @Override
    public void deleteAll() {
        routingMapper.deleteAll();
    }
}
