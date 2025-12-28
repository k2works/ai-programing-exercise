package com.example.production.infrastructure.grpc.service;

import com.example.production.application.port.out.MpsRepository;
import com.example.production.application.service.MrpService;
import com.example.production.grpc.*;
import com.example.production.infrastructure.grpc.mapper.ProtoMapper;
import io.grpc.Status;
import io.grpc.stub.StreamObserver;
import net.devh.boot.grpc.server.service.GrpcService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

/**
 * MRP gRPC サービス実装
 * 既存の MrpService を Input Adapter として呼び出す
 */
@GrpcService
public class MrpGrpcService extends MrpServiceGrpc.MrpServiceImplBase {

    private static final Logger log = LoggerFactory.getLogger(MrpGrpcService.class);

    private final MrpService mrpService;
    private final MpsRepository mpsRepository;
    private final ProtoMapper mapper;

    public MrpGrpcService(MrpService mrpService, MpsRepository mpsRepository, ProtoMapper mapper) {
        this.mrpService = mrpService;
        this.mpsRepository = mpsRepository;
        this.mapper = mapper;
    }

    /**
     * MRP 完全実行（Unary RPC）
     */
    @Override
    @Transactional
    public void executeMrp(ExecuteMrpRequest request,
                           StreamObserver<ExecuteMrpResponse> responseObserver) {
        try {
            mrpService.executeMrp(request.getMpsId(), mpsRepository);

            responseObserver.onNext(ExecuteMrpResponse.newBuilder()
                .setSuccess(true)
                .setMessage("MRP 実行が完了しました")
                .build());
            responseObserver.onCompleted();
        } catch (IllegalArgumentException e) {
            responseObserver.onError(Status.NOT_FOUND
                .withDescription(e.getMessage())
                .asRuntimeException());
        } catch (Exception e) {
            log.error("MRP execution failed", e);
            responseObserver.onError(Status.INTERNAL
                .withDescription(e.getMessage())
                .asRuntimeException());
        }
    }

    /**
     * 所要量展開（Unary RPC）
     */
    @Override
    @Transactional
    public void explodeRequirements(ExplodeRequirementsRequest request,
                                    StreamObserver<ExplodeRequirementsResponse> responseObserver) {
        try {
            List<com.example.production.domain.model.plan.Requirement> requirements =
                mrpService.explodeRequirements(request.getOrderId());

            ExplodeRequirementsResponse.Builder response = ExplodeRequirementsResponse.newBuilder();
            for (com.example.production.domain.model.plan.Requirement requirement : requirements) {
                response.addRequirements(toProtoRequirement(requirement));
            }

            responseObserver.onNext(response.build());
            responseObserver.onCompleted();
        } catch (IllegalArgumentException e) {
            responseObserver.onError(Status.NOT_FOUND
                .withDescription(e.getMessage())
                .asRuntimeException());
        } catch (Exception e) {
            log.error("Requirement explosion failed", e);
            responseObserver.onError(Status.INTERNAL
                .withDescription(e.getMessage())
                .asRuntimeException());
        }
    }

    /**
     * 在庫引当（Unary RPC）
     */
    @Override
    @Transactional
    public void allocateFromInventory(AllocateInventoryRequest request,
                                      StreamObserver<com.example.production.grpc.Allocation> responseObserver) {
        try {
            com.example.production.domain.model.plan.Allocation allocation =
                mrpService.allocateFromInventory(
                    request.getRequirementId(),
                    request.getInventoryQuantity());

            responseObserver.onNext(toProtoAllocation(allocation));
            responseObserver.onCompleted();
        } catch (IllegalArgumentException e) {
            responseObserver.onError(Status.NOT_FOUND
                .withDescription(e.getMessage())
                .asRuntimeException());
        } catch (Exception e) {
            log.error("Allocation failed", e);
            responseObserver.onError(Status.INTERNAL
                .withDescription(e.getMessage())
                .asRuntimeException());
        }
    }

    /**
     * 不足オーダ生成（Unary RPC）
     */
    @Override
    @Transactional
    public void createShortageOrder(CreateShortageOrderRequest request,
                                    StreamObserver<com.example.production.grpc.Order> responseObserver) {
        try {
            com.example.production.domain.model.plan.Order order = mrpService.createShortageOrder(
                request.getItemCode(),
                mapper.toDomainDecimal(request.getShortageQuantity()),
                mapper.toDomainDate(request.getDueDate()),
                request.getLocationCode(),
                toDomainOrderType(request.getOrderType()));

            responseObserver.onNext(toProtoOrder(order));
            responseObserver.onCompleted();
        } catch (IllegalArgumentException e) {
            responseObserver.onError(Status.NOT_FOUND
                .withDescription(e.getMessage())
                .asRuntimeException());
        } catch (Exception e) {
            log.error("Shortage order creation failed", e);
            responseObserver.onError(Status.INTERNAL
                .withDescription(e.getMessage())
                .asRuntimeException());
        }
    }

    /**
     * ロットサイズ計算（Unary RPC）
     */
    @Override
    public void calculateLotSize(CalculateLotSizeRequest request,
                                 StreamObserver<CalculateLotSizeResponse> responseObserver) {
        try {
            var orderQuantity = mrpService.calculateOrderQuantity(
                mapper.toDomainDecimal(request.getRequiredQuantity()),
                mapper.toDomainDecimalOrNull(request.getMinimumLotSize()),
                mapper.toDomainDecimalOrNull(request.getIncrementLotSize()),
                mapper.toDomainDecimalOrNull(request.getMaximumLotSize()));

            responseObserver.onNext(CalculateLotSizeResponse.newBuilder()
                .setOrderQuantity(mapper.toProtoDecimal(orderQuantity))
                .build());
            responseObserver.onCompleted();
        } catch (Exception e) {
            log.error("Lot size calculation failed", e);
            responseObserver.onError(Status.INTERNAL
                .withDescription(e.getMessage())
                .asRuntimeException());
        }
    }

    /**
     * 所要量ストリーム（Server Streaming RPC）
     */
    @Override
    @Transactional
    public void streamRequirements(ExecuteMrpRequest request,
                                   StreamObserver<RequirementEvent> responseObserver) {
        try {
            // MPS から製品オーダを取得し、所要量展開をストリーミング
            mrpService.executeMrp(request.getMpsId(), mpsRepository);

            // 簡易的な実装：完了イベントを送信
            responseObserver.onNext(RequirementEvent.newBuilder()
                .setEventType("COMPLETED")
                .build());
            responseObserver.onCompleted();
        } catch (IllegalArgumentException e) {
            responseObserver.onError(Status.NOT_FOUND
                .withDescription(e.getMessage())
                .asRuntimeException());
        } catch (Exception e) {
            log.error("Requirement streaming failed", e);
            responseObserver.onError(Status.INTERNAL
                .withDescription(e.getMessage())
                .asRuntimeException());
        }
    }

    // ========== 変換メソッド ==========

    private com.example.production.grpc.Requirement toProtoRequirement(
            com.example.production.domain.model.plan.Requirement domain) {
        return com.example.production.grpc.Requirement.newBuilder()
            .setRequirementNumber(mapper.nullToEmpty(domain.getRequirementNumber()))
            .setOrderId(domain.getOrderId() != null ? domain.getOrderId() : 0)
            .setItemCode(mapper.nullToEmpty(domain.getItemCode()))
            .setDueDate(mapper.toProtoDate(domain.getDueDate()))
            .setRequiredQuantity(mapper.toProtoDecimal(domain.getRequiredQuantity()))
            .setAllocatedQuantity(mapper.toProtoDecimal(domain.getAllocatedQuantity()))
            .setShortageQuantity(mapper.toProtoDecimal(domain.getShortageQuantity()))
            .setLocationCode(mapper.nullToEmpty(domain.getLocationCode()))
            .build();
    }

    private com.example.production.grpc.Order toProtoOrder(
            com.example.production.domain.model.plan.Order domain) {
        return com.example.production.grpc.Order.newBuilder()
            .setId(domain.getId() != null ? domain.getId() : 0)
            .setOrderNumber(mapper.nullToEmpty(domain.getOrderNumber()))
            .setOrderType(toProtoOrderType(domain.getOrderType()))
            .setItemCode(mapper.nullToEmpty(domain.getItemCode()))
            .setStartDate(mapper.toProtoDate(domain.getStartDate()))
            .setDueDate(mapper.toProtoDate(domain.getDueDate()))
            .setPlanQuantity(mapper.toProtoDecimal(domain.getPlanQuantity()))
            .setLocationCode(mapper.nullToEmpty(domain.getLocationCode()))
            .setStatus(toProtoPlanStatus(domain.getStatus()))
            .setMpsId(domain.getMpsId() != null ? domain.getMpsId() : 0)
            .setParentOrderId(domain.getParentOrderId() != null ? domain.getParentOrderId() : 0)
            .build();
    }

    private com.example.production.grpc.Allocation toProtoAllocation(
            com.example.production.domain.model.plan.Allocation domain) {
        return com.example.production.grpc.Allocation.newBuilder()
            .setId(domain.getId() != null ? domain.getId() : 0)
            .setRequirementId(domain.getRequirementId() != null ? domain.getRequirementId() : 0)
            .setAllocationType(toProtoAllocationType(domain.getAllocationType()))
            .setOrderId(domain.getOrderId() != null ? domain.getOrderId() : 0)
            .setAllocationDate(mapper.toProtoDate(domain.getAllocationDate()))
            .setAllocatedQuantity(mapper.toProtoDecimal(domain.getAllocatedQuantity()))
            .setLocationCode(mapper.nullToEmpty(domain.getLocationCode()))
            .build();
    }

    private com.example.production.grpc.OrderType toProtoOrderType(
            com.example.production.domain.model.plan.OrderType domain) {
        if (domain == null) {
            return com.example.production.grpc.OrderType.ORDER_TYPE_UNSPECIFIED;
        }
        return switch (domain) {
            case MANUFACTURING -> com.example.production.grpc.OrderType.MANUFACTURING;
            case PURCHASE -> com.example.production.grpc.OrderType.PURCHASE;
        };
    }

    private com.example.production.domain.model.plan.OrderType toDomainOrderType(
            com.example.production.grpc.OrderType proto) {
        return switch (proto) {
            case MANUFACTURING -> com.example.production.domain.model.plan.OrderType.MANUFACTURING;
            case PURCHASE -> com.example.production.domain.model.plan.OrderType.PURCHASE;
            default -> com.example.production.domain.model.plan.OrderType.MANUFACTURING;
        };
    }

    private com.example.production.grpc.PlanStatus toProtoPlanStatus(
            com.example.production.domain.model.plan.PlanStatus domain) {
        if (domain == null) {
            return com.example.production.grpc.PlanStatus.PLAN_STATUS_UNSPECIFIED;
        }
        return switch (domain) {
            case DRAFT -> com.example.production.grpc.PlanStatus.PLAN_DRAFT;
            case CONFIRMED -> com.example.production.grpc.PlanStatus.PLAN_CONFIRMED;
            case EXPANDED -> com.example.production.grpc.PlanStatus.PLAN_EXPANDED;
            case CANCELLED -> com.example.production.grpc.PlanStatus.PLAN_CANCELLED;
        };
    }

    private com.example.production.grpc.AllocationType toProtoAllocationType(
            com.example.production.domain.model.plan.AllocationType domain) {
        if (domain == null) {
            return com.example.production.grpc.AllocationType.ALLOCATION_TYPE_UNSPECIFIED;
        }
        return switch (domain) {
            case INVENTORY -> com.example.production.grpc.AllocationType.ALLOC_INVENTORY;
            case PURCHASE_ORDER -> com.example.production.grpc.AllocationType.ALLOC_PURCHASE_ORDER;
            case MANUFACTURING_ORDER -> com.example.production.grpc.AllocationType.ALLOC_MANUFACTURING_ORDER;
        };
    }
}
