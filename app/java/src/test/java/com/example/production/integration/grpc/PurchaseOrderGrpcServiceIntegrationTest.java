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
 * PurchaseOrder gRPC サービス統合テスト
 */
@DisplayName("PurchaseOrder gRPC サービス")
class PurchaseOrderGrpcServiceIntegrationTest extends BaseGrpcIntegrationTest {

    @GrpcClient("inProcess")
    private PurchaseOrderServiceGrpc.PurchaseOrderServiceBlockingStub blockingStub;

    @GrpcClient("inProcess")
    private PurchaseOrderServiceGrpc.PurchaseOrderServiceStub asyncStub;

    @Nested
    @DisplayName("GetOrder")
    class GetOrderTests {

        @Test
        @DisplayName("存在しない発注番号でNOT_FOUNDエラーを返す")
        void returnsNotFoundForNonexistentOrder() {
            // Given
            String orderNumber = "NONEXISTENT-PO-999";

            // When/Then
            GetPurchaseOrderRequest request = GetPurchaseOrderRequest.newBuilder()
                .setPurchaseOrderNumber(orderNumber)
                .build();

            assertThatThrownBy(() -> blockingStub.getOrder(request))
                .isInstanceOf(StatusRuntimeException.class)
                .hasMessageContaining("NOT_FOUND");
        }
    }

    @Nested
    @DisplayName("StreamOrders")
    class StreamOrdersTests {

        @Test
        @DisplayName("発注一覧をストリーミングで取得できる（空でも成功）")
        void canStreamOrders() {
            // When
            GetPurchaseOrdersRequest request = GetPurchaseOrdersRequest.newBuilder().build();
            List<com.example.production.grpc.PurchaseOrder> orders = new ArrayList<>();

            blockingStub.streamOrders(request).forEachRemaining(orders::add);

            // Then - ストリーミングが完了することを確認
            assertThat(orders).isNotNull();
        }
    }

    @Nested
    @DisplayName("CreateOrder")
    class CreateOrderTests {

        @Test
        @DisplayName("発注を作成できる")
        void canCreateOrder() {
            // Given
            long timestamp = System.currentTimeMillis();
            CreatePurchaseOrderRequest request = CreatePurchaseOrderRequest.newBuilder()
                .setSupplierCode("SUP001")
                .setOrdererCode("EMP001")
                .setDepartmentCode("DEPT001")
                .setRemarks("テスト発注 " + timestamp)
                .addDetails(CreatePurchaseOrderDetailRequest.newBuilder()
                    .setItemCode("ITEM001")
                    .setOrderQuantity(Decimal.newBuilder().setValue("10").setScale(0).build())
                    .setUnitPrice(Decimal.newBuilder().setValue("100").setScale(0).build())
                    .setDeliveryDate(Date.newBuilder().setYear(2025).setMonth(2).setDay(1).build())
                    .build())
                .build();

            // When
            com.example.production.grpc.PurchaseOrder response = blockingStub.createOrder(request);

            // Then
            assertThat(response).isNotNull();
            assertThat(response.getPurchaseOrderNumber()).isNotEmpty();
            assertThat(response.getSupplierCode()).isEqualTo("SUP001");
            assertThat(response.getStatus()).isEqualTo(PurchaseOrderStatusType.CREATING);
        }
    }

    @Nested
    @DisplayName("ConfirmOrder")
    class ConfirmOrderTests {

        @Test
        @DisplayName("存在しない発注番号でNOT_FOUNDエラーを返す")
        void returnsNotFoundForNonexistentOrder() {
            // Given
            ConfirmPurchaseOrderRequest request = ConfirmPurchaseOrderRequest.newBuilder()
                .setPurchaseOrderNumber("NONEXISTENT-PO-999")
                .build();

            // When/Then
            assertThatThrownBy(() -> blockingStub.confirmOrder(request))
                .isInstanceOf(StatusRuntimeException.class)
                .hasMessageContaining("NOT_FOUND");
        }
    }

    @Nested
    @DisplayName("CancelOrder")
    class CancelOrderTests {

        @Test
        @DisplayName("存在しない発注番号でNOT_FOUNDエラーを返す")
        void returnsNotFoundForNonexistentOrder() {
            // Given
            GetPurchaseOrderRequest request = GetPurchaseOrderRequest.newBuilder()
                .setPurchaseOrderNumber("NONEXISTENT-PO-999")
                .build();

            // When/Then
            assertThatThrownBy(() -> blockingStub.cancelOrder(request))
                .isInstanceOf(StatusRuntimeException.class)
                .hasMessageContaining("NOT_FOUND");
        }
    }

    @Nested
    @DisplayName("RecordReceiving")
    class RecordReceivingTests {

        @Test
        @DisplayName("入荷登録の双方向ストリーミングが動作する")
        void canRecordReceiving() throws InterruptedException {
            // Given
            CountDownLatch latch = new CountDownLatch(1);
            List<RecordReceivingResponse> responses = new ArrayList<>();
            final Throwable[] error = new Throwable[1];

            StreamObserver<RecordReceivingRequest> requestObserver = asyncStub.recordReceiving(
                new StreamObserver<>() {
                    @Override
                    public void onNext(RecordReceivingResponse response) {
                        responses.add(response);
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

            // When - 存在しない発注番号でリクエスト送信（エラーにならずレスポンスが返る）
            requestObserver.onNext(RecordReceivingRequest.newBuilder()
                .setPurchaseOrderNumber("TEST-PO-001")
                .setLineNumber(1)
                .setReceivedQuantity(Decimal.newBuilder().setValue("5").setScale(0).build())
                .setReceivingDate(Date.newBuilder().setYear(2025).setMonth(1).setDay(15).build())
                .build());
            requestObserver.onCompleted();

            // Then
            assertThat(latch.await(10, TimeUnit.SECONDS))
                .as("Request should complete within timeout").isTrue();

            if (error[0] != null) {
                throw new AssertionError("gRPC call failed", error[0]);
            }

            // 双方向ストリーミングが正常に完了することを確認
            assertThat(responses).hasSize(1);
            assertThat(responses.get(0).getPurchaseOrderNumber()).isEqualTo("TEST-PO-001");
        }
    }
}
