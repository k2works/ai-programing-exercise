package com.example.production.infrastructure.grpc.service;

import com.example.production.application.port.in.ItemUseCase;
import com.example.production.application.port.in.command.CreateItemCommand;
import com.example.production.application.port.in.command.UpdateItemCommand;
import com.example.production.domain.exception.ItemNotFoundException;
import com.example.production.domain.model.item.Item;
import com.example.production.domain.model.item.ItemCategory;
import com.example.production.grpc.*;
import com.example.production.infrastructure.grpc.mapper.ProtoMapper;
import io.grpc.Status;
import io.grpc.stub.StreamObserver;
import net.devh.boot.grpc.server.service.GrpcService;

import java.util.List;

/**
 * 品目 gRPC サービス実装
 * 既存の ItemUseCase を Input Adapter として呼び出す
 */
@GrpcService
public class ItemGrpcService extends ItemServiceGrpc.ItemServiceImplBase {

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
            responseObserver.onNext(toProto(item));
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
                items = itemUseCase.getItemsByCategory(toDomainCategory(protoCategory));
            } else {
                items = itemUseCase.getAllItems();
            }

            GetItemsResponse.Builder response = GetItemsResponse.newBuilder();
            for (Item item : items) {
                response.addItems(toProto(item));
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
                responseObserver.onNext(toProto(item));
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
            CreateItemCommand command = CreateItemCommand.builder()
                .itemCode(request.getItemCode())
                .itemName(request.getItemName())
                .category(toDomainCategory(request.getCategory()))
                .unitCode(emptyToNull(request.getUnitCode()))
                .leadTime(request.getLeadTime())
                .safetyLeadTime(request.getSafetyLeadTime())
                .safetyStock(mapper.toDomainDecimal(request.getSafetyStock()))
                .yieldRate(mapper.toDomainDecimal(request.getYieldRate()))
                .minLotSize(mapper.toDomainDecimal(request.getMinLotSize()))
                .lotIncrement(mapper.toDomainDecimal(request.getLotIncrement()))
                .maxLotSize(mapper.toDomainDecimal(request.getMaxLotSize()))
                .shelfLife(request.getShelfLife())
                .build();

            Item created = itemUseCase.createItem(command);
            responseObserver.onNext(toProto(created));
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
                .category(toDomainCategory(request.getCategory()))
                .leadTime(request.getLeadTime())
                .safetyLeadTime(request.getSafetyLeadTime())
                .safetyStock(mapper.toDomainDecimal(request.getSafetyStock()))
                .yieldRate(mapper.toDomainDecimal(request.getYieldRate()))
                .minLotSize(mapper.toDomainDecimal(request.getMinLotSize()))
                .lotIncrement(mapper.toDomainDecimal(request.getLotIncrement()))
                .maxLotSize(mapper.toDomainDecimal(request.getMaxLotSize()))
                .shelfLife(request.getShelfLife())
                .build();

            Item updated = itemUseCase.updateItem(command);
            responseObserver.onNext(toProto(updated));
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
     * ドメインモデル -> Proto メッセージ変換
     */
    private com.example.production.grpc.Item toProto(Item domain) {
        return com.example.production.grpc.Item.newBuilder()
            .setItemCode(nullToEmpty(domain.getItemCode()))
            .setItemName(nullToEmpty(domain.getItemName()))
            .setCategory(toProtoCategory(domain.getItemCategory()))
            .setUnitCode(nullToEmpty(domain.getUnitCode()))
            .setLeadTime(domain.getLeadTime() != null ? domain.getLeadTime() : 0)
            .setSafetyLeadTime(domain.getSafetyLeadTime() != null ? domain.getSafetyLeadTime() : 0)
            .setSafetyStock(mapper.toProtoDecimal(domain.getSafetyStock()))
            .setYieldRate(mapper.toProtoDecimal(domain.getYieldRate()))
            .setMinLotSize(mapper.toProtoDecimal(domain.getMinLotSize()))
            .setLotIncrement(mapper.toProtoDecimal(domain.getLotIncrement()))
            .setMaxLotSize(mapper.toProtoDecimal(domain.getMaxLotSize()))
            .setShelfLife(domain.getShelfLife() != null ? domain.getShelfLife() : 0)
            .build();
    }

    /**
     * ドメインカテゴリ -> Proto カテゴリ変換
     */
    private com.example.production.grpc.ItemCategory toProtoCategory(ItemCategory domain) {
        if (domain == null) {
            return com.example.production.grpc.ItemCategory.ITEM_CATEGORY_UNSPECIFIED;
        }
        return switch (domain) {
            case PRODUCT -> com.example.production.grpc.ItemCategory.PRODUCT;
            case SEMI_PRODUCT -> com.example.production.grpc.ItemCategory.SEMI_PRODUCT;
            case PART -> com.example.production.grpc.ItemCategory.COMPONENT;
            case RAW_MATERIAL, MATERIAL -> com.example.production.grpc.ItemCategory.RAW_MATERIAL;
            case SUPPLY -> com.example.production.grpc.ItemCategory.PURCHASED_PART;
            case INTERMEDIATE -> com.example.production.grpc.ItemCategory.SEMI_PRODUCT;
        };
    }

    /**
     * Proto カテゴリ -> ドメインカテゴリ変換
     */
    private ItemCategory toDomainCategory(com.example.production.grpc.ItemCategory proto) {
        return switch (proto) {
            case PRODUCT -> ItemCategory.PRODUCT;
            case SEMI_PRODUCT -> ItemCategory.SEMI_PRODUCT;
            case COMPONENT -> ItemCategory.PART;
            case RAW_MATERIAL -> ItemCategory.RAW_MATERIAL;
            case PURCHASED_PART -> ItemCategory.SUPPLY;
            default -> ItemCategory.PRODUCT;
        };
    }

    private String nullToEmpty(String value) {
        return value == null ? "" : value;
    }

    private String emptyToNull(String value) {
        return value == null || value.isEmpty() ? null : value;
    }
}
