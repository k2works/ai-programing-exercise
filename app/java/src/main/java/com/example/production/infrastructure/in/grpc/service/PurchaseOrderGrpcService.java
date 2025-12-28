package com.example.production.infrastructure.in.grpc.service;

import com.example.production.application.port.in.PurchaseOrderUseCase;
import com.example.production.application.port.in.command.CreatePurchaseOrderCommand;
import com.example.production.domain.exception.PurchaseOrderNotFoundException;
import com.example.production.domain.model.purchase.PurchaseOrder;
import com.example.production.domain.model.purchase.PurchaseOrderDetail;
import com.example.production.domain.model.purchase.PurchaseOrderStatus;
import com.example.production.grpc.*;
import com.example.production.infrastructure.in.grpc.mapper.ProtoMapper;
import io.grpc.Status;
import io.grpc.stub.StreamObserver;
import net.devh.boot.grpc.server.service.GrpcService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

/**
 * 発注 gRPC サービス実装
 * 既存の PurchaseOrderUseCase を Input Adapter として呼び出す
 */
@GrpcService
public class PurchaseOrderGrpcService extends PurchaseOrderServiceGrpc.PurchaseOrderServiceImplBase {

    private static final Logger log = LoggerFactory.getLogger(PurchaseOrderGrpcService.class);

    private final PurchaseOrderUseCase purchaseOrderUseCase;
    private final ProtoMapper mapper;

    public PurchaseOrderGrpcService(PurchaseOrderUseCase purchaseOrderUseCase, ProtoMapper mapper) {
        this.purchaseOrderUseCase = purchaseOrderUseCase;
        this.mapper = mapper;
    }

    /**
     * 発注取得（Unary RPC）
     */
    @Override
    public void getOrder(GetPurchaseOrderRequest request,
                         StreamObserver<com.example.production.grpc.PurchaseOrder> responseObserver) {
        try {
            PurchaseOrder order = purchaseOrderUseCase.getOrder(request.getPurchaseOrderNumber());
            responseObserver.onNext(toProto(order));
            responseObserver.onCompleted();
        } catch (PurchaseOrderNotFoundException e) {
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
     * 発注一覧ストリーム（Server Streaming RPC）
     */
    @Override
    public void streamOrders(GetPurchaseOrdersRequest request,
                             StreamObserver<com.example.production.grpc.PurchaseOrder> responseObserver) {
        try {
            List<PurchaseOrder> orders = purchaseOrderUseCase.getAllOrders();

            for (PurchaseOrder order : orders) {
                responseObserver.onNext(toProto(order));
            }
            responseObserver.onCompleted();
        } catch (Exception e) {
            responseObserver.onError(Status.INTERNAL
                .withDescription(e.getMessage())
                .asRuntimeException());
        }
    }

    /**
     * 発注作成（Unary RPC）
     */
    @Override
    @Transactional
    public void createOrder(CreatePurchaseOrderRequest request,
                            StreamObserver<com.example.production.grpc.PurchaseOrder> responseObserver) {
        try {
            CreatePurchaseOrderCommand command = CreatePurchaseOrderCommand.builder()
                .supplierCode(request.getSupplierCode())
                .ordererCode(mapper.emptyToNull(request.getOrdererCode()))
                .departmentCode(mapper.emptyToNull(request.getDepartmentCode()))
                .remarks(mapper.emptyToNull(request.getRemarks()))
                .details(request.getDetailsList().stream()
                    .map(d -> CreatePurchaseOrderCommand.PurchaseOrderDetailCommand.builder()
                        .itemCode(d.getItemCode())
                        .orderQuantity(mapper.toDomainDecimal(d.getOrderQuantity()))
                        .unitPrice(mapper.toDomainDecimal(d.getUnitPrice()))
                        .deliveryDate(mapper.toDomainDate(d.getDeliveryDate()))
                        .build())
                    .toList())
                .build();

            PurchaseOrder order = purchaseOrderUseCase.createOrder(command);
            responseObserver.onNext(toProto(order));
            responseObserver.onCompleted();
        } catch (Exception e) {
            responseObserver.onError(Status.INTERNAL
                .withDescription(e.getMessage())
                .asRuntimeException());
        }
    }

    /**
     * 発注確定（Unary RPC）
     */
    @Override
    @Transactional
    public void confirmOrder(ConfirmPurchaseOrderRequest request,
                             StreamObserver<com.example.production.grpc.PurchaseOrder> responseObserver) {
        try {
            PurchaseOrder order = purchaseOrderUseCase.confirmOrder(request.getPurchaseOrderNumber());
            responseObserver.onNext(toProto(order));
            responseObserver.onCompleted();
        } catch (PurchaseOrderNotFoundException e) {
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
     * 発注取消（Unary RPC）
     */
    @Override
    @Transactional
    public void cancelOrder(GetPurchaseOrderRequest request,
                            StreamObserver<Empty> responseObserver) {
        try {
            purchaseOrderUseCase.cancelOrder(request.getPurchaseOrderNumber());
            responseObserver.onNext(Empty.getDefaultInstance());
            responseObserver.onCompleted();
        } catch (PurchaseOrderNotFoundException e) {
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
     * 入荷登録（Bidirectional Streaming RPC）
     */
    @Override
    public StreamObserver<RecordReceivingRequest> recordReceiving(
            StreamObserver<RecordReceivingResponse> responseObserver) {

        return new StreamObserver<>() {
            @Override
            public void onNext(RecordReceivingRequest request) {
                try {
                    // 入荷処理を実行
                    PurchaseOrder order = purchaseOrderUseCase.receiveOrder(request.getPurchaseOrderNumber());

                    // 対象の明細を取得して結果を返す
                    PurchaseOrderDetail detail = order.getDetails().stream()
                        .filter(d -> d.getLineNumber().equals(request.getLineNumber()))
                        .findFirst()
                        .orElse(null);

                    if (detail != null) {
                        responseObserver.onNext(RecordReceivingResponse.newBuilder()
                            .setPurchaseOrderNumber(request.getPurchaseOrderNumber())
                            .setLineNumber(request.getLineNumber())
                            .setTotalReceived(mapper.toProtoDecimal(detail.getReceivedQuantity()))
                            .setIsCompleted(detail.getCompletedFlag() != null && detail.getCompletedFlag())
                            .build());
                    } else {
                        responseObserver.onNext(RecordReceivingResponse.newBuilder()
                            .setPurchaseOrderNumber(request.getPurchaseOrderNumber())
                            .setLineNumber(request.getLineNumber())
                            .setIsCompleted(false)
                            .build());
                    }
                } catch (Exception e) {
                    log.warn("Failed to record receiving: {}", e.getMessage());
                    responseObserver.onNext(RecordReceivingResponse.newBuilder()
                        .setPurchaseOrderNumber(request.getPurchaseOrderNumber())
                        .setLineNumber(request.getLineNumber())
                        .setIsCompleted(false)
                        .build());
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
                responseObserver.onCompleted();
            }
        };
    }

    /**
     * ドメインモデル -> Proto メッセージ変換
     */
    private com.example.production.grpc.PurchaseOrder toProto(PurchaseOrder domain) {
        var builder = com.example.production.grpc.PurchaseOrder.newBuilder()
            .setPurchaseOrderNumber(mapper.nullToEmpty(domain.getPurchaseOrderNumber()))
            .setSupplierCode(mapper.nullToEmpty(domain.getSupplierCode()))
            .setOrderDate(mapper.toProtoDate(domain.getOrderDate()))
            .setStatus(toProtoStatus(domain.getStatus()))
            .setTotalAmount(mapper.toProtoDecimal(domain.getTotalOrderAmount()))
            .setOrdererCode(mapper.nullToEmpty(domain.getOrdererCode()))
            .setDepartmentCode(mapper.nullToEmpty(domain.getDepartmentCode()))
            .setRemarks(mapper.nullToEmpty(domain.getRemarks()));

        if (domain.getDetails() != null) {
            for (PurchaseOrderDetail detail : domain.getDetails()) {
                builder.addDetails(toProtoDetail(detail));
            }
        }

        return builder.build();
    }

    /**
     * 明細のドメイン -> Proto 変換
     */
    private PurchaseOrderDetailMsg toProtoDetail(PurchaseOrderDetail domain) {
        return PurchaseOrderDetailMsg.newBuilder()
            .setLineNumber(domain.getLineNumber() != null ? domain.getLineNumber() : 0)
            .setItemCode(mapper.nullToEmpty(domain.getItemCode()))
            .setOrderQuantity(mapper.toProtoDecimal(domain.getOrderQuantity()))
            .setOrderUnitPrice(mapper.toProtoDecimal(domain.getOrderUnitPrice()))
            .setExpectedReceivingDate(mapper.toProtoDate(domain.getExpectedReceivingDate()))
            .setReceivedQuantity(mapper.toProtoDecimal(domain.getReceivedQuantity()))
            .setOrderAmount(mapper.toProtoDecimal(domain.getOrderAmount()))
            .setCompletedFlag(domain.getCompletedFlag() != null && domain.getCompletedFlag())
            .build();
    }

    /**
     * ステータスの変換
     */
    private PurchaseOrderStatusType toProtoStatus(PurchaseOrderStatus domain) {
        if (domain == null) {
            return PurchaseOrderStatusType.PURCHASE_ORDER_STATUS_UNSPECIFIED;
        }
        return switch (domain) {
            case CREATING -> PurchaseOrderStatusType.CREATING;
            case ORDERED -> PurchaseOrderStatusType.ORDERED;
            case PARTIALLY_RECEIVED -> PurchaseOrderStatusType.PARTIALLY_RECEIVED;
            case RECEIVED -> PurchaseOrderStatusType.RECEIVED;
            case ACCEPTED -> PurchaseOrderStatusType.ACCEPTED;
            case CANCELLED -> PurchaseOrderStatusType.CANCELLED;
        };
    }
}
