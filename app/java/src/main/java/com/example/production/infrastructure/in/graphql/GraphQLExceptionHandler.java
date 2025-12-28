package com.example.production.infrastructure.in.graphql;

import com.example.production.domain.exception.*;
import graphql.GraphQLError;
import graphql.GraphqlErrorBuilder;
import graphql.schema.DataFetchingEnvironment;
import org.springframework.graphql.execution.DataFetcherExceptionResolverAdapter;
import org.springframework.graphql.execution.ErrorType;
import org.springframework.stereotype.Component;

/**
 * GraphQL グローバル例外ハンドラ
 * ドメイン例外を適切な GraphQL エラーに変換する
 */
@Component
public class GraphQLExceptionHandler extends DataFetcherExceptionResolverAdapter {

    @Override
    protected GraphQLError resolveToSingleError(Throwable ex, DataFetchingEnvironment env) {
        // 品目が見つからない
        if (ex instanceof ItemNotFoundException) {
            return GraphqlErrorBuilder.newError(env)
                .errorType(ErrorType.NOT_FOUND)
                .message(ex.getMessage())
                .build();
        }

        // 発注が見つからない
        if (ex instanceof PurchaseOrderNotFoundException) {
            return GraphqlErrorBuilder.newError(env)
                .errorType(ErrorType.NOT_FOUND)
                .message(ex.getMessage())
                .build();
        }

        // 作業指示が見つからない
        if (ex instanceof WorkOrderNotFoundException) {
            return GraphqlErrorBuilder.newError(env)
                .errorType(ErrorType.NOT_FOUND)
                .message(ex.getMessage())
                .build();
        }

        // 品目コード重複
        if (ex instanceof DuplicateItemException) {
            return GraphqlErrorBuilder.newError(env)
                .errorType(ErrorType.BAD_REQUEST)
                .message(ex.getMessage())
                .build();
        }

        // 在庫不足
        if (ex instanceof InsufficientInventoryException) {
            return GraphqlErrorBuilder.newError(env)
                .errorType(ErrorType.BAD_REQUEST)
                .message(ex.getMessage())
                .build();
        }

        // ドメイン例外（汎用）
        if (ex instanceof DomainException) {
            return GraphqlErrorBuilder.newError(env)
                .errorType(ErrorType.BAD_REQUEST)
                .message(ex.getMessage())
                .build();
        }

        // 不正な引数
        if (ex instanceof IllegalArgumentException) {
            return GraphqlErrorBuilder.newError(env)
                .errorType(ErrorType.BAD_REQUEST)
                .message(ex.getMessage())
                .build();
        }

        // 不正な状態
        if (ex instanceof IllegalStateException) {
            return GraphqlErrorBuilder.newError(env)
                .errorType(ErrorType.BAD_REQUEST)
                .message(ex.getMessage())
                .build();
        }

        // 未知のエラー
        return GraphqlErrorBuilder.newError(env)
            .errorType(ErrorType.INTERNAL_ERROR)
            .message("内部エラーが発生しました")
            .build();
    }
}
