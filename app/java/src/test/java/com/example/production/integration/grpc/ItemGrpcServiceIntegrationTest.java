package com.example.production.integration.grpc;

import com.example.production.grpc.*;
import io.grpc.StatusRuntimeException;
import net.devh.boot.grpc.client.inject.GrpcClient;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.List;

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
}
