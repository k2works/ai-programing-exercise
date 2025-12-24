package com.example.production.infrastructure.persistence.repository;

import com.example.production.application.port.out.MpsRepository;
import com.example.production.application.port.out.OrderRepository;
import com.example.production.domain.model.plan.MasterProductionSchedule;
import com.example.production.domain.model.plan.Order;
import com.example.production.domain.model.plan.OrderType;
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

@DisplayName("オーダリポジトリ")
class OrderRepositoryTest extends BaseIntegrationTest {

    @Autowired
    private OrderRepository orderRepository;

    @Autowired
    private MpsRepository mpsRepository;

    @BeforeEach
    void setUp() {
        orderRepository.deleteAll();
        mpsRepository.deleteAll();
    }

    private MasterProductionSchedule createMps(String mpsNumber) {
        var mps = MasterProductionSchedule.builder()
                .mpsNumber(mpsNumber)
                .planDate(LocalDate.of(2025, 1, 15))
                .itemCode("ITEM001")
                .planQuantity(new BigDecimal("100"))
                .dueDate(LocalDate.of(2025, 2, 1))
                .build();
        mpsRepository.save(mps);
        return mps;
    }

    @Nested
    @DisplayName("登録")
    class Registration {

        @Test
        @DisplayName("製造オーダを登録できる")
        void canRegisterManufacturingOrder() {
            // Arrange
            var mps = createMps("MPS-001");
            var order = Order.builder()
                    .orderNumber("MO-001")
                    .orderType(OrderType.MANUFACTURING)
                    .itemCode("ITEM001")
                    .startDate(LocalDate.of(2025, 1, 20))
                    .dueDate(LocalDate.of(2025, 2, 1))
                    .planQuantity(new BigDecimal("100"))
                    .locationCode("LOC001")
                    .mpsId(mps.getId())
                    .createdBy("system")
                    .build();

            // Act
            orderRepository.save(order);

            // Assert
            assertThat(order.getId()).isNotNull();

            var result = orderRepository.findByOrderNumber("MO-001");
            assertThat(result).isPresent();
            assertThat(result.get().getOrderType()).isEqualTo(OrderType.MANUFACTURING);
            assertThat(result.get().getStatus()).isEqualTo(PlanStatus.DRAFT);
        }

        @Test
        @DisplayName("購買オーダを登録できる")
        void canRegisterPurchaseOrder() {
            // Arrange
            var order = Order.builder()
                    .orderNumber("PO-001")
                    .orderType(OrderType.PURCHASE)
                    .itemCode("MAT001")
                    .startDate(LocalDate.of(2025, 1, 15))
                    .dueDate(LocalDate.of(2025, 1, 25))
                    .planQuantity(new BigDecimal("500"))
                    .locationCode("LOC001")
                    .createdBy("system")
                    .build();

            // Act
            orderRepository.save(order);

            // Assert
            var result = orderRepository.findByOrderNumber("PO-001");
            assertThat(result).isPresent();
            assertThat(result.get().getOrderType()).isEqualTo(OrderType.PURCHASE);
        }
    }

    @Nested
    @DisplayName("検索")
    class Search {

        @Test
        @DisplayName("MPS IDで検索できる")
        void canFindByMpsId() {
            // Arrange
            var mps = createMps("MPS-002");
            var order1 = Order.builder()
                    .orderNumber("MO-002")
                    .orderType(OrderType.MANUFACTURING)
                    .itemCode("ITEM001")
                    .startDate(LocalDate.of(2025, 1, 20))
                    .dueDate(LocalDate.of(2025, 2, 1))
                    .planQuantity(new BigDecimal("100"))
                    .locationCode("LOC001")
                    .mpsId(mps.getId())
                    .build();
            orderRepository.save(order1);

            var order2 = Order.builder()
                    .orderNumber("MO-003")
                    .orderType(OrderType.MANUFACTURING)
                    .itemCode("ITEM002")
                    .startDate(LocalDate.of(2025, 1, 22))
                    .dueDate(LocalDate.of(2025, 2, 1))
                    .planQuantity(new BigDecimal("50"))
                    .locationCode("LOC001")
                    .mpsId(mps.getId())
                    .build();
            orderRepository.save(order2);

            // Act
            var result = orderRepository.findByMpsId(mps.getId());

            // Assert
            assertThat(result).hasSize(2);
        }

        @Test
        @DisplayName("親オーダIDで検索できる")
        void canFindByParentOrderId() {
            // Arrange
            var parentOrder = Order.builder()
                    .orderNumber("MO-PARENT")
                    .orderType(OrderType.MANUFACTURING)
                    .itemCode("ITEM001")
                    .startDate(LocalDate.of(2025, 1, 20))
                    .dueDate(LocalDate.of(2025, 2, 1))
                    .planQuantity(new BigDecimal("100"))
                    .locationCode("LOC001")
                    .build();
            orderRepository.save(parentOrder);

            var childOrder = Order.builder()
                    .orderNumber("PO-CHILD")
                    .orderType(OrderType.PURCHASE)
                    .itemCode("MAT001")
                    .startDate(LocalDate.of(2025, 1, 15))
                    .dueDate(LocalDate.of(2025, 1, 20))
                    .planQuantity(new BigDecimal("200"))
                    .locationCode("LOC001")
                    .parentOrderId(parentOrder.getId())
                    .build();
            orderRepository.save(childOrder);

            // Act
            var result = orderRepository.findByParentOrderId(parentOrder.getId());

            // Assert
            assertThat(result).hasSize(1);
            assertThat(result.get(0).getOrderNumber()).isEqualTo("PO-CHILD");
        }
    }

    @Nested
    @DisplayName("更新")
    class Update {

        @Test
        @DisplayName("親オーダIDを更新できる")
        void canUpdateParentOrderId() {
            // Arrange
            var parentOrder = Order.builder()
                    .orderNumber("MO-P1")
                    .orderType(OrderType.MANUFACTURING)
                    .itemCode("ITEM001")
                    .startDate(LocalDate.of(2025, 1, 20))
                    .dueDate(LocalDate.of(2025, 2, 1))
                    .planQuantity(new BigDecimal("100"))
                    .locationCode("LOC001")
                    .build();
            orderRepository.save(parentOrder);

            var childOrder = Order.builder()
                    .orderNumber("PO-C1")
                    .orderType(OrderType.PURCHASE)
                    .itemCode("MAT001")
                    .startDate(LocalDate.of(2025, 1, 15))
                    .dueDate(LocalDate.of(2025, 1, 20))
                    .planQuantity(new BigDecimal("200"))
                    .locationCode("LOC001")
                    .build();
            orderRepository.save(childOrder);

            // Act
            orderRepository.updateParentOrderId(childOrder.getId(), parentOrder.getId());

            // Assert
            var result = orderRepository.findById(childOrder.getId());
            assertThat(result).isPresent();
            assertThat(result.get().getParentOrderId()).isEqualTo(parentOrder.getId());
        }

        @Test
        @DisplayName("ステータスを更新できる")
        void canUpdateStatus() {
            // Arrange
            var order = Order.builder()
                    .orderNumber("MO-S1")
                    .orderType(OrderType.MANUFACTURING)
                    .itemCode("ITEM001")
                    .startDate(LocalDate.of(2025, 1, 20))
                    .dueDate(LocalDate.of(2025, 2, 1))
                    .planQuantity(new BigDecimal("100"))
                    .locationCode("LOC001")
                    .status(PlanStatus.DRAFT)
                    .build();
            orderRepository.save(order);

            // Act
            orderRepository.updateStatus(order.getId(), PlanStatus.CONFIRMED);

            // Assert
            var result = orderRepository.findById(order.getId());
            assertThat(result).isPresent();
            assertThat(result.get().getStatus()).isEqualTo(PlanStatus.CONFIRMED);
        }
    }
}
