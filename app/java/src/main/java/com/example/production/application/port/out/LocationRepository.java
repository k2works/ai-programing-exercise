package com.example.production.application.port.out;

import com.example.production.domain.model.location.Location;
import com.example.production.domain.model.location.LocationType;

import java.util.List;
import java.util.Optional;

/**
 * 場所リポジトリ（Output Port）
 */
public interface LocationRepository {

    void save(Location location);

    Optional<Location> findByLocationCode(String locationCode);

    List<Location> findByLocationType(LocationType locationType);

    List<Location> findByParentLocationCode(String parentLocationCode);

    List<Location> findAll();

    void deleteAll();
}
