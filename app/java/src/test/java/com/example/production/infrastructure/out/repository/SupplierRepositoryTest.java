package com.example.production.infrastructure.out.repository;

import com.example.production.application.port.out.SupplierRepository;
import com.example.production.domain.model.supplier.Supplier;
import com.example.production.domain.model.supplier.SupplierType;
import com.example.production.testsetup.BaseIntegrationTest;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;

import java.time.LocalDate;

import static org.assertj.core.api.Assertions.assertThat;

@DisplayName("取引先リポジトリ")
class SupplierRepositoryTest extends BaseIntegrationTest {

    @Autowired
    private SupplierRepository supplierRepository;

    @BeforeEach
    void setUp() {
        supplierRepository.deleteAll();
    }

    @Nested
    @DisplayName("登録")
    class Registration {

        @Test
        @DisplayName("取引先を登録できる")
        void canRegisterSupplier() {
            // Arrange
            var supplier = Supplier.builder()
                    .supplierCode("SUP-001")
                    .effectiveFrom(LocalDate.of(2025, 1, 1))
                    .supplierName("株式会社テスト")
                    .supplierNameKana("カブシキガイシャテスト")
                    .supplierType(SupplierType.VENDOR)
                    .postalCode("100-0001")
                    .address("東京都千代田区")
                    .phoneNumber("03-1234-5678")
                    .build();

            // Act
            supplierRepository.save(supplier);

            // Assert
            var result = supplierRepository.findBySupplierCode("SUP-001");
            assertThat(result).isPresent();
            assertThat(result.get().getSupplierName()).isEqualTo("株式会社テスト");
            assertThat(result.get().getSupplierType()).isEqualTo(SupplierType.VENDOR);
        }

        @Test
        @DisplayName("全ての取引先区分を登録できる")
        void canRegisterAllSupplierTypes() {
            var types = SupplierType.values();

            for (int i = 0; i < types.length; i++) {
                var supplier = Supplier.builder()
                        .supplierCode("SUP-" + String.format("%03d", i))
                        .effectiveFrom(LocalDate.of(2025, 1, 1))
                        .supplierName("取引先" + types[i].getDisplayName())
                        .supplierType(types[i])
                        .build();

                supplierRepository.save(supplier);

                var result = supplierRepository.findBySupplierCode(supplier.getSupplierCode());
                assertThat(result).isPresent();
                assertThat(result.get().getSupplierType()).isEqualTo(types[i]);
            }
        }
    }

    @Nested
    @DisplayName("世代管理")
    class VersionManagement {

        @Test
        @DisplayName("基準日時点の有効な取引先を取得できる")
        void canFindBySupplierCodeAndDate() {
            // Arrange: 世代1（旧）
            var oldVersion = Supplier.builder()
                    .supplierCode("SUP-001")
                    .effectiveFrom(LocalDate.of(2024, 1, 1))
                    .effectiveTo(LocalDate.of(2025, 3, 31))
                    .supplierName("旧取引先名")
                    .supplierType(SupplierType.VENDOR)
                    .build();
            supplierRepository.save(oldVersion);

            // 世代2（新）
            var newVersion = Supplier.builder()
                    .supplierCode("SUP-001")
                    .effectiveFrom(LocalDate.of(2025, 4, 1))
                    .supplierName("新取引先名")
                    .supplierType(SupplierType.VENDOR)
                    .build();
            supplierRepository.save(newVersion);

            // Act & Assert: 2025年2月時点 → 旧バージョン
            var feb = supplierRepository.findBySupplierCodeAndDate("SUP-001", LocalDate.of(2025, 2, 1));
            assertThat(feb).isPresent();
            assertThat(feb.get().getSupplierName()).isEqualTo("旧取引先名");

            // 2025年5月時点 → 新バージョン
            var may = supplierRepository.findBySupplierCodeAndDate("SUP-001", LocalDate.of(2025, 5, 1));
            assertThat(may).isPresent();
            assertThat(may.get().getSupplierName()).isEqualTo("新取引先名");
        }
    }

    @Nested
    @DisplayName("区分別検索")
    class SearchByType {

        @Test
        @DisplayName("取引先区分で検索できる")
        void canFindBySupplierType() {
            // Arrange
            supplierRepository.save(Supplier.builder()
                    .supplierCode("SUP-001")
                    .effectiveFrom(LocalDate.of(2025, 1, 1))
                    .supplierName("仕入先A")
                    .supplierType(SupplierType.VENDOR)
                    .build());
            supplierRepository.save(Supplier.builder()
                    .supplierCode("SUP-002")
                    .effectiveFrom(LocalDate.of(2025, 1, 1))
                    .supplierName("仕入先B")
                    .supplierType(SupplierType.VENDOR)
                    .build());
            supplierRepository.save(Supplier.builder()
                    .supplierCode("SUP-003")
                    .effectiveFrom(LocalDate.of(2025, 1, 1))
                    .supplierName("外注先A")
                    .supplierType(SupplierType.SUBCONTRACTOR)
                    .build());

            // Act
            var vendors = supplierRepository.findBySupplierType(SupplierType.VENDOR);

            // Assert
            assertThat(vendors).hasSize(2);
            assertThat(vendors).allMatch(s -> s.getSupplierType() == SupplierType.VENDOR);
        }
    }
}
