package com.example.production.infrastructure.persistence.repository;

import com.example.production.application.port.out.AllocationRepository;
import com.example.production.application.port.out.OrderRepository;
import com.example.production.application.port.out.RequirementRepository;
import com.example.production.domain.model.plan.Allocation;
import com.example.production.domain.model.plan.AllocationType;
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

@DisplayName("引当リポジトリ")
class AllocationRepositoryTest extends BaseIntegrationTest {

    @Autowired
    private AllocationRepository allocationRepository;

    @Autowired
    private RequirementRepository requirementRepository;

    @Autowired
    private OrderRepository orderRepository;

    @BeforeEach
    void setUp() {
        allocationRepository.deleteAll();
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

    private Requirement createRequirement(Order order, String requirementNumber) {
        var requirement = Requirement.builder()
                .requirementNumber(requirementNumber)
                .orderId(order.getId())
                .itemCode("MAT001")
                .dueDate(LocalDate.of(2025, 1, 20))
                .requiredQuantity(new BigDecimal("200"))
                .locationCode("LOC001")
                .build();
        requirementRepository.save(requirement);
        return requirement;
    }

    @Nested
    @DisplayName("登録")
    class Registration {

        @Test
        @DisplayName("在庫引当を登録できる")
        void canRegisterInventoryAllocation() {
            // Arrange
            var order = createOrder("MO-001");
            var requirement = createRequirement(order, "REQ-001");
            var allocation = Allocation.builder()
                    .requirementId(requirement.getId())
                    .allocationType(AllocationType.INVENTORY)
                    .allocationDate(LocalDate.of(2025, 1, 15))
                    .allocatedQuantity(new BigDecimal("100"))
                    .locationCode("LOC001")
                    .build();

            // Act
            allocationRepository.save(allocation);

            // Assert
            assertThat(allocation.getId()).isNotNull();

            var result = allocationRepository.findByRequirementId(requirement.getId());
            assertThat(result).hasSize(1);
            assertThat(result.get(0).getAllocationType()).isEqualTo(AllocationType.INVENTORY);
            assertThat(result.get(0).getAllocatedQuantity()).isEqualByComparingTo(new BigDecimal("100"));
        }

        @Test
        @DisplayName("発注残引当を登録できる")
        void canRegisterPurchaseOrderAllocation() {
            // Arrange
            var order = createOrder("MO-002");
            var requirement = createRequirement(order, "REQ-002");

            var purchaseOrder = Order.builder()
                    .orderNumber("PO-001")
                    .orderType(OrderType.PURCHASE)
                    .itemCode("MAT001")
                    .startDate(LocalDate.of(2025, 1, 10))
                    .dueDate(LocalDate.of(2025, 1, 18))
                    .planQuantity(new BigDecimal("300"))
                    .locationCode("LOC001")
                    .build();
            orderRepository.save(purchaseOrder);

            var allocation = Allocation.builder()
                    .requirementId(requirement.getId())
                    .allocationType(AllocationType.PURCHASE_ORDER)
                    .orderId(purchaseOrder.getId())
                    .allocationDate(LocalDate.of(2025, 1, 18))
                    .allocatedQuantity(new BigDecimal("100"))
                    .locationCode("LOC001")
                    .build();

            // Act
            allocationRepository.save(allocation);

            // Assert
            var result = allocationRepository.findByRequirementId(requirement.getId());
            assertThat(result).hasSize(1);
            assertThat(result.get(0).getAllocationType()).isEqualTo(AllocationType.PURCHASE_ORDER);
            assertThat(result.get(0).getOrderId()).isEqualTo(purchaseOrder.getId());
        }

        @Test
        @DisplayName("製造残引当を登録できる")
        void canRegisterManufacturingOrderAllocation() {
            // Arrange
            var order = createOrder("MO-003");
            var requirement = createRequirement(order, "REQ-003");

            var manufacturingOrder = Order.builder()
                    .orderNumber("MO-CHILD")
                    .orderType(OrderType.MANUFACTURING)
                    .itemCode("SEMI001")
                    .startDate(LocalDate.of(2025, 1, 12))
                    .dueDate(LocalDate.of(2025, 1, 19))
                    .planQuantity(new BigDecimal("150"))
                    .locationCode("LOC001")
                    .build();
            orderRepository.save(manufacturingOrder);

            var allocation = Allocation.builder()
                    .requirementId(requirement.getId())
                    .allocationType(AllocationType.MANUFACTURING_ORDER)
                    .orderId(manufacturingOrder.getId())
                    .allocationDate(LocalDate.of(2025, 1, 19))
                    .allocatedQuantity(new BigDecimal("150"))
                    .locationCode("LOC001")
                    .build();

            // Act
            allocationRepository.save(allocation);

            // Assert
            var result = allocationRepository.findByRequirementId(requirement.getId());
            assertThat(result).hasSize(1);
            assertThat(result.get(0).getAllocationType()).isEqualTo(AllocationType.MANUFACTURING_ORDER);
        }
    }

    @Nested
    @DisplayName("検索")
    class Search {

        @Test
        @DisplayName("所要IDで複数の引当を検索できる")
        void canFindMultipleAllocationsByRequirementId() {
            // Arrange
            var order = createOrder("MO-004");
            var requirement = createRequirement(order, "REQ-004");

            var allocation1 = Allocation.builder()
                    .requirementId(requirement.getId())
                    .allocationType(AllocationType.INVENTORY)
                    .allocationDate(LocalDate.of(2025, 1, 15))
                    .allocatedQuantity(new BigDecimal("50"))
                    .locationCode("LOC001")
                    .build();
            allocationRepository.save(allocation1);

            var allocation2 = Allocation.builder()
                    .requirementId(requirement.getId())
                    .allocationType(AllocationType.PURCHASE_ORDER)
                    .allocationDate(LocalDate.of(2025, 1, 18))
                    .allocatedQuantity(new BigDecimal("150"))
                    .locationCode("LOC001")
                    .build();
            allocationRepository.save(allocation2);

            // Act
            var result = allocationRepository.findByRequirementId(requirement.getId());

            // Assert
            assertThat(result).hasSize(2);
        }
    }

    @Nested
    @DisplayName("引当区分")
    class AllocationTypes {

        @Test
        @DisplayName("全ての引当区分を登録できる")
        void canRegisterAllAllocationTypes() {
            var order = createOrder("MO-005");
            var requirement = createRequirement(order, "REQ-005");

            for (var allocationType : AllocationType.values()) {
                var allocation = Allocation.builder()
                        .requirementId(requirement.getId())
                        .allocationType(allocationType)
                        .allocationDate(LocalDate.of(2025, 1, 15))
                        .allocatedQuantity(new BigDecimal("10"))
                        .locationCode("LOC001")
                        .build();

                allocationRepository.save(allocation);
            }

            var result = allocationRepository.findByRequirementId(requirement.getId());
            assertThat(result).hasSize(AllocationType.values().length);
        }
    }
}
