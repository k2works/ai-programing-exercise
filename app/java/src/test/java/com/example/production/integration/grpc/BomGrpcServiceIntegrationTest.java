package com.example.production.integration.grpc;

import com.example.production.grpc.*;
import net.devh.boot.grpc.client.inject.GrpcClient;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * BOM gRPC サービス統合テスト
 */
@DisplayName("BOM gRPC サービス")
class BomGrpcServiceIntegrationTest extends BaseGrpcIntegrationTest {

    @GrpcClient("inProcess")
    private BomServiceGrpc.BomServiceBlockingStub blockingStub;

    @Nested
    @DisplayName("ExplodeBom")
    class ExplodeBomTests {

        @Test
        @DisplayName("部品展開をストリーミングで取得できる（空でも成功）")
        void canExplodeBom() {
            // Given
            ExplodeBomRequest request = ExplodeBomRequest.newBuilder()
                .setItemCode("TEST-PRODUCT")
                .setQuantity(Decimal.newBuilder().setValue("1").build())
                .build();

            // When
            List<BomNode> nodes = new ArrayList<>();
            blockingStub.explodeBom(request).forEachRemaining(nodes::add);

            // Then - ストリーミングが完了することを確認
            assertThat(nodes).isNotNull();
        }
    }

    @Nested
    @DisplayName("WhereUsed")
    class WhereUsedTests {

        @Test
        @DisplayName("使用先照会を取得できる（空でも成功）")
        void canGetWhereUsed() {
            // Given
            WhereUsedRequest request = WhereUsedRequest.newBuilder()
                .setItemCode("TEST-PART")
                .build();

            // When
            WhereUsedResponse response = blockingStub.whereUsed(request);

            // Then - 応答が返ることを確認
            assertThat(response).isNotNull();
        }
    }

    @Nested
    @DisplayName("GetChildren")
    class GetChildrenTests {

        @Test
        @DisplayName("子品目一覧を取得できる（空でも成功）")
        void canGetChildren() {
            // Given
            GetChildrenRequest request = GetChildrenRequest.newBuilder()
                .setParentItemCode("TEST-PRODUCT")
                .build();

            // When
            GetChildrenResponse response = blockingStub.getChildren(request);

            // Then - 応答が返ることを確認
            assertThat(response).isNotNull();
        }
    }
}
