package com.example.production.infrastructure.out.repository;

import com.example.production.application.port.out.MpsRepository;
import com.example.production.domain.model.plan.MasterProductionSchedule;
import com.example.production.domain.model.plan.PlanStatus;
import com.example.production.testsetup.BaseIntegrationTest;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;

import java.math.BigDecimal;
import java.time.LocalDate;

import static org.assertj.core.api.Assertions.assertThat;

@DisplayName("基準生産計画リポジトリ")
class MpsRepositoryTest extends BaseIntegrationTest {

    @Autowired
    private MpsRepository mpsRepository;

    @BeforeEach
    void setUp() {
        mpsRepository.deleteAll();
    }

    @Nested
    @DisplayName("登録")
    class Registration {

        @Test
        @DisplayName("基準生産計画を登録できる")
        void canRegisterMps() {
            // Arrange
            var mps = MasterProductionSchedule.builder()
                    .mpsNumber("MPS-001")
                    .planDate(LocalDate.of(2025, 1, 15))
                    .itemCode("ITEM001")
                    .planQuantity(new BigDecimal("100"))
                    .dueDate(LocalDate.of(2025, 2, 1))
                    .locationCode("LOC001")
                    .createdBy("system")
                    .build();

            // Act
            mpsRepository.save(mps);

            // Assert
            assertThat(mps.getId()).isNotNull();

            var result = mpsRepository.findByMpsNumber("MPS-001");
            assertThat(result).isPresent();
            assertThat(result.get().getItemCode()).isEqualTo("ITEM001");
            assertThat(result.get().getPlanQuantity()).isEqualByComparingTo(new BigDecimal("100"));
            assertThat(result.get().getStatus()).isEqualTo(PlanStatus.DRAFT);
        }
    }

    @Nested
    @DisplayName("検索")
    class Search {

        @Test
        @DisplayName("IDで検索できる")
        void canFindById() {
            // Arrange
            var mps = MasterProductionSchedule.builder()
                    .mpsNumber("MPS-002")
                    .planDate(LocalDate.of(2025, 1, 15))
                    .itemCode("ITEM001")
                    .planQuantity(new BigDecimal("200"))
                    .dueDate(LocalDate.of(2025, 2, 15))
                    .build();
            mpsRepository.save(mps);

            // Act
            var result = mpsRepository.findById(mps.getId());

            // Assert
            assertThat(result).isPresent();
            assertThat(result.get().getMpsNumber()).isEqualTo("MPS-002");
        }

        @Test
        @DisplayName("ステータスで検索できる")
        void canFindByStatus() {
            // Arrange
            var mps1 = MasterProductionSchedule.builder()
                    .mpsNumber("MPS-003")
                    .planDate(LocalDate.of(2025, 1, 15))
                    .itemCode("ITEM001")
                    .planQuantity(new BigDecimal("100"))
                    .dueDate(LocalDate.of(2025, 2, 1))
                    .status(PlanStatus.DRAFT)
                    .build();
            mpsRepository.save(mps1);

            var mps2 = MasterProductionSchedule.builder()
                    .mpsNumber("MPS-004")
                    .planDate(LocalDate.of(2025, 1, 20))
                    .itemCode("ITEM002")
                    .planQuantity(new BigDecimal("200"))
                    .dueDate(LocalDate.of(2025, 2, 10))
                    .status(PlanStatus.CONFIRMED)
                    .build();
            mpsRepository.save(mps2);

            // Act
            var draftList = mpsRepository.findByStatus(PlanStatus.DRAFT);
            var confirmedList = mpsRepository.findByStatus(PlanStatus.CONFIRMED);

            // Assert
            assertThat(draftList).hasSize(1);
            assertThat(draftList.get(0).getMpsNumber()).isEqualTo("MPS-003");

            assertThat(confirmedList).hasSize(1);
            assertThat(confirmedList.get(0).getMpsNumber()).isEqualTo("MPS-004");
        }
    }

    @Nested
    @DisplayName("ステータス更新")
    class StatusUpdate {

        @Test
        @DisplayName("ステータスを更新できる")
        void canUpdateStatus() {
            // Arrange
            var mps = MasterProductionSchedule.builder()
                    .mpsNumber("MPS-005")
                    .planDate(LocalDate.of(2025, 1, 15))
                    .itemCode("ITEM001")
                    .planQuantity(new BigDecimal("100"))
                    .dueDate(LocalDate.of(2025, 2, 1))
                    .status(PlanStatus.DRAFT)
                    .build();
            mpsRepository.save(mps);

            // Act
            mpsRepository.updateStatus(mps.getId(), PlanStatus.CONFIRMED);

            // Assert
            var result = mpsRepository.findById(mps.getId());
            assertThat(result).isPresent();
            assertThat(result.get().getStatus()).isEqualTo(PlanStatus.CONFIRMED);
        }

        @Test
        @DisplayName("全てのステータスを設定できる")
        void canSetAllStatuses() {
            for (var status : PlanStatus.values()) {
                var mps = MasterProductionSchedule.builder()
                        .mpsNumber("MPS-STATUS-" + status.name())
                        .planDate(LocalDate.of(2025, 1, 15))
                        .itemCode("ITEM001")
                        .planQuantity(new BigDecimal("100"))
                        .dueDate(LocalDate.of(2025, 2, 1))
                        .status(status)
                        .build();

                mpsRepository.save(mps);

                var result = mpsRepository.findByMpsNumber(mps.getMpsNumber());
                assertThat(result).isPresent();
                assertThat(result.get().getStatus()).isEqualTo(status);
            }
        }
    }
}
