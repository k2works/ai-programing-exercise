package com.example.production.infrastructure.persistence.mapper;

import com.example.production.domain.model.location.Location;
import com.example.production.domain.model.location.LocationType;
import org.apache.ibatis.annotations.Mapper;

import java.util.List;
import java.util.Optional;

@Mapper
public interface LocationMapper {
    void insert(Location location);
    Optional<Location> findByLocationCode(String locationCode);
    List<Location> findByLocationType(LocationType locationType);
    List<Location> findByParentLocationCode(String parentLocationCode);
    List<Location> findAll();
    void deleteAll();
}
