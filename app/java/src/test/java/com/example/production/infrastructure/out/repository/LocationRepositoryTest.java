package com.example.production.infrastructure.out.repository;

import com.example.production.application.port.out.LocationRepository;
import com.example.production.domain.model.location.Location;
import com.example.production.domain.model.location.LocationType;
import com.example.production.testsetup.BaseIntegrationTest;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;

import static org.assertj.core.api.Assertions.assertThat;

@DisplayName("場所リポジトリ")
class LocationRepositoryTest extends BaseIntegrationTest {

    @Autowired
    private LocationRepository locationRepository;

    @BeforeEach
    void setUp() {
        locationRepository.deleteAll();
    }

    @Nested
    @DisplayName("登録")
    class Registration {

        @Test
        @DisplayName("場所を登録できる")
        void canRegisterLocation() {
            // Arrange
            var location = Location.builder()
                    .locationCode("WH-001")
                    .locationName("第1倉庫")
                    .locationType(LocationType.WAREHOUSE)
                    .build();

            // Act
            locationRepository.save(location);

            // Assert
            var result = locationRepository.findByLocationCode("WH-001");
            assertThat(result).isPresent();
            assertThat(result.get().getLocationName()).isEqualTo("第1倉庫");
            assertThat(result.get().getLocationType()).isEqualTo(LocationType.WAREHOUSE);
        }

        @Test
        @DisplayName("全ての場所区分を登録できる")
        void canRegisterAllLocationTypes() {
            var types = LocationType.values();

            for (int i = 0; i < types.length; i++) {
                var location = Location.builder()
                        .locationCode("LOC-" + String.format("%03d", i))
                        .locationName("場所" + types[i].getDisplayName())
                        .locationType(types[i])
                        .build();

                locationRepository.save(location);

                var result = locationRepository.findByLocationCode(location.getLocationCode());
                assertThat(result).isPresent();
                assertThat(result.get().getLocationType()).isEqualTo(types[i]);
            }
        }
    }

    @Nested
    @DisplayName("親子関係")
    class Hierarchy {

        @Test
        @DisplayName("親場所を指定して子場所を検索できる")
        void canFindByParentLocation() {
            // Arrange: 親場所を作成
            var parent = Location.builder()
                    .locationCode("FACTORY-1")
                    .locationName("第1工場")
                    .locationType(LocationType.MANUFACTURING)
                    .build();
            locationRepository.save(parent);

            // 子場所を作成
            locationRepository.save(Location.builder()
                    .locationCode("LINE-1A")
                    .locationName("ライン1A")
                    .locationType(LocationType.MANUFACTURING)
                    .parentLocationCode("FACTORY-1")
                    .build());

            locationRepository.save(Location.builder()
                    .locationCode("LINE-1B")
                    .locationName("ライン1B")
                    .locationType(LocationType.MANUFACTURING)
                    .parentLocationCode("FACTORY-1")
                    .build());

            // Act
            var children = locationRepository.findByParentLocationCode("FACTORY-1");

            // Assert
            assertThat(children).hasSize(2);
            assertThat(children.stream().map(Location::getLocationCode).toList())
                    .containsExactlyInAnyOrder("LINE-1A", "LINE-1B");
        }
    }

    @Nested
    @DisplayName("区分別検索")
    class SearchByType {

        @Test
        @DisplayName("場所区分で検索できる")
        void canFindByLocationType() {
            // Arrange
            locationRepository.save(Location.builder()
                    .locationCode("WH-001")
                    .locationName("第1倉庫")
                    .locationType(LocationType.WAREHOUSE)
                    .build());
            locationRepository.save(Location.builder()
                    .locationCode("WH-002")
                    .locationName("第2倉庫")
                    .locationType(LocationType.WAREHOUSE)
                    .build());
            locationRepository.save(Location.builder()
                    .locationCode("MFG-001")
                    .locationName("製造エリア1")
                    .locationType(LocationType.MANUFACTURING)
                    .build());

            // Act
            var warehouses = locationRepository.findByLocationType(LocationType.WAREHOUSE);

            // Assert
            assertThat(warehouses).hasSize(2);
            assertThat(warehouses).allMatch(l -> l.getLocationType() == LocationType.WAREHOUSE);
        }
    }
}
