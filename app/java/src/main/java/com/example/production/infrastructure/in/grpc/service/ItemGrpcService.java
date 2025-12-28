package com.example.production.infrastructure.in.grpc.service;

import com.example.production.application.port.in.ItemUseCase;
import com.example.production.application.port.in.command.UpdateItemCommand;
import com.example.production.domain.exception.ItemNotFoundException;
import com.example.production.domain.model.item.Item;
import com.example.production.grpc.*;
import com.example.production.infrastructure.in.grpc.mapper.ProtoMapper;
import io.grpc.Status;
import io.grpc.stub.StreamObserver;
import net.devh.boot.grpc.server.service.GrpcService;
import org.springframework.transaction.annotation.Transactional;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.List;

/**
 * 品目 gRPC サービス実装
 * 既存の ItemUseCase を Input Adapter として呼び出す
 */
@GrpcService
public class ItemGrpcService extends ItemServiceGrpc.ItemServiceImplBase {

    private static final Logger log = LoggerFactory.getLogger(ItemGrpcService.class);

    private final ItemUseCase itemUseCase;
    private final ProtoMapper mapper;

    public ItemGrpcService(ItemUseCase itemUseCase, ProtoMapper mapper) {
        this.itemUseCase = itemUseCase;
        this.mapper = mapper;
    }

    /**
     * 品目取得（Unary RPC）
     */
    @Override
    public void getItem(GetItemRequest request,
                        StreamObserver<com.example.production.grpc.Item> responseObserver) {
        try {
            Item item = itemUseCase.getItemByCode(request.getItemCode());
            responseObserver.onNext(mapper.toProto(item));
            responseObserver.onCompleted();
        } catch (ItemNotFoundException e) {
            responseObserver.onError(Status.NOT_FOUND
                .withDescription(e.getMessage())
                .asRuntimeException());
        } catch (Exception e) {
            responseObserver.onError(Status.INTERNAL
                .withDescription(e.getMessage())
                .asRuntimeException());
        }
    }

    /**
     * 品目一覧取得（Unary RPC）
     */
    @Override
    public void getItems(GetItemsRequest request,
                         StreamObserver<GetItemsResponse> responseObserver) {
        try {
            List<Item> items;
            var protoCategory = request.getCategory();
            if (protoCategory != com.example.production.grpc.ItemCategory.ITEM_CATEGORY_UNSPECIFIED) {
                items = itemUseCase.getItemsByCategory(mapper.toDomainCategory(protoCategory));
            } else {
                items = itemUseCase.getAllItems();
            }

            GetItemsResponse.Builder response = GetItemsResponse.newBuilder();
            for (Item item : items) {
                response.addItems(mapper.toProto(item));
            }

            responseObserver.onNext(response.build());
            responseObserver.onCompleted();
        } catch (Exception e) {
            responseObserver.onError(Status.INTERNAL
                .withDescription(e.getMessage())
                .asRuntimeException());
        }
    }

    /**
     * 品目ストリーム（Server Streaming RPC）
     */
    @Override
    public void streamItems(GetItemsRequest request,
                            StreamObserver<com.example.production.grpc.Item> responseObserver) {
        try {
            List<Item> items = itemUseCase.getAllItems();

            for (Item item : items) {
                responseObserver.onNext(mapper.toProto(item));
            }
            responseObserver.onCompleted();
        } catch (Exception e) {
            responseObserver.onError(Status.INTERNAL
                .withDescription(e.getMessage())
                .asRuntimeException());
        }
    }

    /**
     * 品目作成（Unary RPC）
     */
    @Override
    public void createItem(CreateItemRequest request,
                           StreamObserver<com.example.production.grpc.Item> responseObserver) {
        try {
            Item created = itemUseCase.createItem(mapper.toCommand(request));
            responseObserver.onNext(mapper.toProto(created));
            responseObserver.onCompleted();
        } catch (Exception e) {
            responseObserver.onError(Status.INTERNAL
                .withDescription(e.getMessage())
                .asRuntimeException());
        }
    }

    /**
     * 品目更新（Unary RPC）
     */
    @Override
    public void updateItem(UpdateItemRequest request,
                           StreamObserver<com.example.production.grpc.Item> responseObserver) {
        try {
            UpdateItemCommand command = UpdateItemCommand.builder()
                .itemCode(request.getItemCode())
                .itemName(request.getItemName())
                .category(mapper.toDomainCategory(request.getCategory()))
                .leadTime(mapper.zeroToNull(request.getLeadTime()))
                .safetyLeadTime(mapper.zeroToNull(request.getSafetyLeadTime()))
                .safetyStock(mapper.toDomainDecimal(request.getSafetyStock()))
                .yieldRate(mapper.toDomainDecimal(request.getYieldRate()))
                .minLotSize(mapper.toDomainDecimal(request.getMinLotSize()))
                .lotIncrement(mapper.toDomainDecimal(request.getLotIncrement()))
                .maxLotSize(mapper.toDomainDecimal(request.getMaxLotSize()))
                .shelfLife(mapper.zeroToNull(request.getShelfLife()))
                .build();

            Item updated = itemUseCase.updateItem(command);
            responseObserver.onNext(mapper.toProto(updated));
            responseObserver.onCompleted();
        } catch (ItemNotFoundException e) {
            responseObserver.onError(Status.NOT_FOUND
                .withDescription(e.getMessage())
                .asRuntimeException());
        } catch (Exception e) {
            responseObserver.onError(Status.INTERNAL
                .withDescription(e.getMessage())
                .asRuntimeException());
        }
    }

    /**
     * 品目削除（Unary RPC）
     */
    @Override
    public void deleteItem(DeleteItemRequest request,
                           StreamObserver<Empty> responseObserver) {
        try {
            itemUseCase.deleteItem(request.getItemCode());
            responseObserver.onNext(Empty.getDefaultInstance());
            responseObserver.onCompleted();
        } catch (ItemNotFoundException e) {
            responseObserver.onError(Status.NOT_FOUND
                .withDescription(e.getMessage())
                .asRuntimeException());
        } catch (Exception e) {
            responseObserver.onError(Status.INTERNAL
                .withDescription(e.getMessage())
                .asRuntimeException());
        }
    }

    /**
     * バッチ登録（Client Streaming RPC）
     */
    @Override
    @Transactional
    public StreamObserver<CreateItemRequest> batchCreateItems(
            StreamObserver<BatchCreateResponse> responseObserver) {

        return new StreamObserver<>() {
            private final List<String> successCodes = new ArrayList<>();
            private final List<String> failedCodes = new ArrayList<>();

            @Override
            public void onNext(CreateItemRequest request) {
                try {
                    itemUseCase.createItem(mapper.toCommand(request));
                    successCodes.add(request.getItemCode());
                } catch (Exception e) {
                    log.warn("Failed to create item: {}, error: {}", request.getItemCode(), e.getMessage());
                    failedCodes.add(request.getItemCode());
                }
            }

            @Override
            public void onError(Throwable t) {
                responseObserver.onError(Status.INTERNAL
                    .withDescription(t.getMessage())
                    .asRuntimeException());
            }

            @Override
            public void onCompleted() {
                responseObserver.onNext(BatchCreateResponse.newBuilder()
                    .setSuccessCount(successCodes.size())
                    .setFailedCount(failedCodes.size())
                    .addAllFailedCodes(failedCodes)
                    .build());
                responseObserver.onCompleted();
            }
        };
    }
}
