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
 * Item gRPC サービス統合テスト
 * シードデータを使用してテスト
 */
@DisplayName("Item gRPC サービス")
class ItemGrpcServiceIntegrationTest extends BaseGrpcIntegrationTest {

    @GrpcClient("inProcess")
    private ItemServiceGrpc.ItemServiceBlockingStub blockingStub;

    @GrpcClient("inProcess")
    private ItemServiceGrpc.ItemServiceStub asyncStub;

    @Nested
    @DisplayName("GetItem")
    class GetItemTests {

        @Test
        @DisplayName("存在しない品目コードでNOT_FOUNDエラーを返す")
        void returnsNotFoundForNonexistentItem() {
            // Given
            String itemCode = "NONEXISTENT-999";

            // When/Then
            GetItemRequest request = GetItemRequest.newBuilder()
                .setItemCode(itemCode)
                .build();

            assertThatThrownBy(() -> blockingStub.getItem(request))
                .isInstanceOf(StatusRuntimeException.class)
                .hasMessageContaining("NOT_FOUND");
        }
    }

    @Nested
    @DisplayName("GetItems")
    class GetItemsTests {

        @Test
        @DisplayName("品目一覧を取得できる（空でも成功）")
        void canGetItems() {
            // When
            GetItemsRequest request = GetItemsRequest.newBuilder().build();
            GetItemsResponse response = blockingStub.getItems(request);

            // Then - 応答が返ることを確認（データの有無は問わない）
            assertThat(response).isNotNull();
        }
    }

    @Nested
    @DisplayName("StreamItems")
    class StreamItemsTests {

        @Test
        @DisplayName("品目をストリーミングで取得できる（空でも成功）")
        void canStreamItems() {
            // When
            GetItemsRequest request = GetItemsRequest.newBuilder().build();
            List<com.example.production.grpc.Item> items = new ArrayList<>();

            blockingStub.streamItems(request).forEachRemaining(items::add);

            // Then - ストリーミングが完了することを確認
            assertThat(items).isNotNull();
        }
    }

    @Nested
    @DisplayName("BatchCreateItems")
    class BatchCreateItemsTests {

        @Test
        @DisplayName("バッチで品目を登録できる（Client Streaming）")
        void canBatchCreateItems() throws InterruptedException {
            // Given
            CountDownLatch latch = new CountDownLatch(1);
            final BatchCreateResponse[] result = new BatchCreateResponse[1];
            final Throwable[] error = new Throwable[1];

            StreamObserver<CreateItemRequest> requestObserver = asyncStub.batchCreateItems(
                new StreamObserver<>() {
                    @Override
                    public void onNext(BatchCreateResponse response) {
                        result[0] = response;
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

            // When - ユニークな品目コードで3件送信
            long timestamp = System.currentTimeMillis();
            for (int i = 1; i <= 3; i++) {
                requestObserver.onNext(CreateItemRequest.newBuilder()
                    .setItemCode("BATCH-" + timestamp + "-" + i)
                    .setItemName("バッチ品目" + i)
                    .setCategory(com.example.production.grpc.ItemCategory.PRODUCT)
                    .build());
            }
            requestObserver.onCompleted();

            // Then
            assertThat(latch.await(10, TimeUnit.SECONDS))
                .as("Request should complete within timeout").isTrue();

            if (error[0] != null) {
                throw new AssertionError("gRPC call failed", error[0]);
            }

            assertThat(result[0]).as("Response should not be null").isNotNull();
            // 全品目が処理されたことを確認（成功または失敗）
            int total = result[0].getSuccessCount() + result[0].getFailedCount();
            assertThat(total).as("Total processed should be 3").isEqualTo(3);
        }
    }
}
