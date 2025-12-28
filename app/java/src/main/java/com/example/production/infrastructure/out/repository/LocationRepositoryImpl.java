package com.example.production.infrastructure.out.repository;

import com.example.production.application.port.out.LocationRepository;
import com.example.production.domain.model.location.Location;
import com.example.production.domain.model.location.LocationType;
import com.example.production.infrastructure.out.mapper.LocationMapper;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
public class LocationRepositoryImpl implements LocationRepository {

    private final LocationMapper locationMapper;

    public LocationRepositoryImpl(LocationMapper locationMapper) {
        this.locationMapper = locationMapper;
    }

    @Override
    public void save(Location location) {
        locationMapper.insert(location);
    }

    @Override
    public Optional<Location> findByLocationCode(String locationCode) {
        return locationMapper.findByLocationCode(locationCode);
    }

    @Override
    public List<Location> findByLocationType(LocationType locationType) {
        return locationMapper.findByLocationType(locationType);
    }

    @Override
    public List<Location> findByParentLocationCode(String parentLocationCode) {
        return locationMapper.findByParentLocationCode(parentLocationCode);
    }

    @Override
    public List<Location> findAll() {
        return locationMapper.findAll();
    }

    @Override
    public void deleteAll() {
        locationMapper.deleteAll();
    }
}
