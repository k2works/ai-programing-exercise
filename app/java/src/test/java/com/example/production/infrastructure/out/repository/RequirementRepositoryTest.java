package com.example.production.infrastructure.out.repository;

import com.example.production.application.port.out.OrderRepository;
import com.example.production.application.port.out.RequirementRepository;
import com.example.production.domain.model.plan.Order;
import com.example.production.domain.model.plan.OrderType;
import com.example.production.domain.model.plan.Requirement;
import com.example.production.testsetup.BaseIntegrationTest;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;

import java.math.BigDecimal;
import java.time.LocalDate;

import static org.assertj.core.api.Assertions.assertThat;

@DisplayName("所要リポジトリ")
class RequirementRepositoryTest extends BaseIntegrationTest {

    @Autowired
    private RequirementRepository requirementRepository;

    @Autowired
    private OrderRepository orderRepository;

    @BeforeEach
    void setUp() {
        requirementRepository.deleteAll();
        orderRepository.deleteAll();
    }

    private Order createOrder(String orderNumber) {
        var order = Order.builder()
                .orderNumber(orderNumber)
                .orderType(OrderType.MANUFACTURING)
                .itemCode("ITEM001")
                .startDate(LocalDate.of(2025, 1, 20))
                .dueDate(LocalDate.of(2025, 2, 1))
                .planQuantity(new BigDecimal("100"))
                .locationCode("LOC001")
                .build();
        orderRepository.save(order);
        return order;
    }

    @Nested
    @DisplayName("登録")
    class Registration {

        @Test
        @DisplayName("所要を登録できる")
        void canRegisterRequirement() {
            // Arrange
            var order = createOrder("MO-001");
            var requirement = Requirement.builder()
                    .requirementNumber("REQ-001")
                    .orderId(order.getId())
                    .itemCode("MAT001")
                    .dueDate(LocalDate.of(2025, 1, 20))
                    .requiredQuantity(new BigDecimal("200"))
                    .locationCode("LOC001")
                    .build();

            // Act
            requirementRepository.save(requirement);

            // Assert
            assertThat(requirement.getId()).isNotNull();

            var result = requirementRepository.findById(requirement.getId());
            assertThat(result).isPresent();
            assertThat(result.get().getItemCode()).isEqualTo("MAT001");
            assertThat(result.get().getRequiredQuantity()).isEqualByComparingTo(new BigDecimal("200"));
            assertThat(result.get().getAllocatedQuantity()).isEqualByComparingTo(BigDecimal.ZERO);
            assertThat(result.get().getShortageQuantity()).isEqualByComparingTo(BigDecimal.ZERO);
        }
    }

    @Nested
    @DisplayName("検索")
    class Search {

        @Test
        @DisplayName("オーダIDで検索できる")
        void canFindByOrderId() {
            // Arrange
            var order = createOrder("MO-002");
            var req1 = Requirement.builder()
                    .requirementNumber("REQ-002")
                    .orderId(order.getId())
                    .itemCode("MAT001")
                    .dueDate(LocalDate.of(2025, 1, 20))
                    .requiredQuantity(new BigDecimal("100"))
                    .locationCode("LOC001")
                    .build();
            requirementRepository.save(req1);

            var req2 = Requirement.builder()
                    .requirementNumber("REQ-003")
                    .orderId(order.getId())
                    .itemCode("MAT002")
                    .dueDate(LocalDate.of(2025, 1, 20))
                    .requiredQuantity(new BigDecimal("50"))
                    .locationCode("LOC001")
                    .build();
            requirementRepository.save(req2);

            // Act
            var result = requirementRepository.findByOrderId(order.getId());

            // Assert
            assertThat(result).hasSize(2);
        }
    }

    @Nested
    @DisplayName("引当更新")
    class AllocationUpdate {

        @Test
        @DisplayName("引当済数量を更新できる")
        void canUpdateAllocatedQuantity() {
            // Arrange
            var order = createOrder("MO-003");
            var requirement = Requirement.builder()
                    .requirementNumber("REQ-004")
                    .orderId(order.getId())
                    .itemCode("MAT001")
                    .dueDate(LocalDate.of(2025, 1, 20))
                    .requiredQuantity(new BigDecimal("200"))
                    .locationCode("LOC001")
                    .build();
            requirementRepository.save(requirement);

            // Act
            requirementRepository.updateAllocation(
                    requirement.getId(),
                    new BigDecimal("150"),
                    new BigDecimal("50")
            );

            // Assert
            var result = requirementRepository.findById(requirement.getId());
            assertThat(result).isPresent();
            assertThat(result.get().getAllocatedQuantity()).isEqualByComparingTo(new BigDecimal("150"));
            assertThat(result.get().getShortageQuantity()).isEqualByComparingTo(new BigDecimal("50"));
        }
    }
}
