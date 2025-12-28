package com.example.production.integration.grpc;

import com.example.production.grpc.*;
import io.grpc.StatusRuntimeException;
import io.grpc.stub.StreamObserver;
import net.devh.boot.grpc.client.inject.GrpcClient;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

/**
 * MRP gRPC サービス統合テスト
 */
@DisplayName("MRP gRPC サービス")
class MrpGrpcServiceIntegrationTest extends BaseGrpcIntegrationTest {

    @GrpcClient("inProcess")
    private MrpServiceGrpc.MrpServiceBlockingStub blockingStub;

    @GrpcClient("inProcess")
    private MrpServiceGrpc.MrpServiceStub asyncStub;

    @Nested
    @DisplayName("ExecuteMrp")
    class ExecuteMrpTests {

        @Test
        @DisplayName("存在しないMPS IDでNOT_FOUNDエラーを返す")
        void returnsNotFoundForNonexistentMps() {
            // Given
            ExecuteMrpRequest request = ExecuteMrpRequest.newBuilder()
                .setMpsId(99999)
                .build();

            // When/Then
            assertThatThrownBy(() -> blockingStub.executeMrp(request))
                .isInstanceOf(StatusRuntimeException.class)
                .hasMessageContaining("NOT_FOUND");
        }
    }

    @Nested
    @DisplayName("ExplodeRequirements")
    class ExplodeRequirementsTests {

        @Test
        @DisplayName("存在しないオーダIDでNOT_FOUNDエラーを返す")
        void returnsNotFoundForNonexistentOrder() {
            // Given
            ExplodeRequirementsRequest request = ExplodeRequirementsRequest.newBuilder()
                .setOrderId(99999)
                .build();

            // When/Then
            assertThatThrownBy(() -> blockingStub.explodeRequirements(request))
                .isInstanceOf(StatusRuntimeException.class)
                .hasMessageContaining("NOT_FOUND");
        }
    }

    @Nested
    @DisplayName("AllocateFromInventory")
    class AllocateFromInventoryTests {

        @Test
        @DisplayName("存在しない所要IDでNOT_FOUNDエラーを返す")
        void returnsNotFoundForNonexistentRequirement() {
            // Given
            AllocateInventoryRequest request = AllocateInventoryRequest.newBuilder()
                .setRequirementId(99999)
                .setInventoryQuantity(10)
                .build();

            // When/Then
            assertThatThrownBy(() -> blockingStub.allocateFromInventory(request))
                .isInstanceOf(StatusRuntimeException.class)
                .hasMessageContaining("NOT_FOUND");
        }
    }

    @Nested
    @DisplayName("CreateShortageOrder")
    class CreateShortageOrderTests {

        @Test
        @DisplayName("存在しない品目コードでNOT_FOUNDエラーを返す")
        void returnsNotFoundForNonexistentItem() {
            // Given
            CreateShortageOrderRequest request = CreateShortageOrderRequest.newBuilder()
                .setItemCode("NONEXISTENT-ITEM-999")
                .setShortageQuantity(Decimal.newBuilder().setValue("100").setScale(0).build())
                .setDueDate(Date.newBuilder().setYear(2025).setMonth(2).setDay(1).build())
                .setLocationCode("WH-001")
                .setOrderType(OrderType.MANUFACTURING)
                .build();

            // When/Then
            assertThatThrownBy(() -> blockingStub.createShortageOrder(request))
                .isInstanceOf(StatusRuntimeException.class)
                .hasMessageContaining("NOT_FOUND");
        }
    }

    @Nested
    @DisplayName("CalculateLotSize")
    class CalculateLotSizeTests {

        @Test
        @DisplayName("ロットサイズを正しく計算できる")
        void canCalculateLotSize() {
            // Given - 所要量: 15, 最小ロット: 10, 刻み: 5, 最大ロット: 100
            CalculateLotSizeRequest request = CalculateLotSizeRequest.newBuilder()
                .setRequiredQuantity(Decimal.newBuilder().setValue("15").setScale(0).build())
                .setMinimumLotSize(Decimal.newBuilder().setValue("10").setScale(0).build())
                .setIncrementLotSize(Decimal.newBuilder().setValue("5").setScale(0).build())
                .setMaximumLotSize(Decimal.newBuilder().setValue("100").setScale(0).build())
                .build();

            // When
            CalculateLotSizeResponse response = blockingStub.calculateLotSize(request);

            // Then - 10 + ceil((15-10)/5)*5 = 10 + 5 = 15
            assertThat(response).isNotNull();
            assertThat(response.getOrderQuantity().getValue()).isEqualTo("15");
        }

        @Test
        @DisplayName("最小ロットサイズ未満の場合、最小ロットを返す")
        void returnsMinimumLotWhenBelowMinimum() {
            // Given - 所要量: 5, 最小ロット: 10
            CalculateLotSizeRequest request = CalculateLotSizeRequest.newBuilder()
                .setRequiredQuantity(Decimal.newBuilder().setValue("5").setScale(0).build())
                .setMinimumLotSize(Decimal.newBuilder().setValue("10").setScale(0).build())
                .setIncrementLotSize(Decimal.newBuilder().setValue("5").setScale(0).build())
                .build();

            // When
            CalculateLotSizeResponse response = blockingStub.calculateLotSize(request);

            // Then
            assertThat(response).isNotNull();
            assertThat(response.getOrderQuantity().getValue()).isEqualTo("10");
        }
    }

    @Nested
    @DisplayName("StreamRequirements")
    class StreamRequirementsTests {

        @Test
        @DisplayName("存在しないMPS IDでNOT_FOUNDエラーを返す")
        void returnsNotFoundForNonexistentMps() throws InterruptedException {
            // Given
            CountDownLatch latch = new CountDownLatch(1);
            List<RequirementEvent> events = new ArrayList<>();
            final Throwable[] error = new Throwable[1];

            // When
            asyncStub.streamRequirements(
                ExecuteMrpRequest.newBuilder().setMpsId(99999).build(),
                new StreamObserver<>() {
                    @Override
                    public void onNext(RequirementEvent event) {
                        events.add(event);
                    }

                    @Override
                    public void onError(Throwable t) {
                        error[0] = t;
                        latch.countDown();
                    }

                    @Override
                    public void onCompleted() {
                        latch.countDown();
                    }
                });

            // Then
            assertThat(latch.await(10, TimeUnit.SECONDS)).isTrue();
            assertThat(error[0]).isNotNull();
            assertThat(error[0]).isInstanceOf(StatusRuntimeException.class);
            assertThat(error[0].getMessage()).contains("NOT_FOUND");
        }
    }
}
