package com.example.production.infrastructure.grpc.service;

import com.example.production.application.port.out.BomRepository;
import com.example.production.domain.model.bom.Bom;
import com.example.production.domain.model.bom.BomExplosion;
import com.example.production.grpc.*;
import com.example.production.infrastructure.grpc.mapper.ProtoMapper;
import io.grpc.Status;
import io.grpc.stub.StreamObserver;
import net.devh.boot.grpc.server.service.GrpcService;

import java.math.BigDecimal;
import java.util.List;

/**
 * BOM gRPC サービス実装
 * 既存の BomRepository を Input Adapter として呼び出す
 */
@GrpcService
public class BomGrpcService extends BomServiceGrpc.BomServiceImplBase {

    private final BomRepository bomRepository;
    private final ProtoMapper mapper;

    public BomGrpcService(BomRepository bomRepository, ProtoMapper mapper) {
        this.bomRepository = bomRepository;
        this.mapper = mapper;
    }

    /**
     * 部品展開（Server Streaming）
     */
    @Override
    public void explodeBom(ExplodeBomRequest request,
                           StreamObserver<BomNode> responseObserver) {
        try {
            BigDecimal quantity = mapper.toDomainDecimal(request.getQuantity());
            if (quantity == null || quantity.compareTo(BigDecimal.ZERO) <= 0) {
                quantity = BigDecimal.ONE;
            }

            List<BomExplosion> explosions = bomRepository.explode(request.getItemCode(), quantity);

            // ストリーミング配信
            for (BomExplosion explosion : explosions) {
                responseObserver.onNext(toBomNode(explosion));
            }
            responseObserver.onCompleted();
        } catch (Exception e) {
            responseObserver.onError(Status.INTERNAL
                .withDescription(e.getMessage())
                .asRuntimeException());
        }
    }

    /**
     * 使用先照会
     */
    @Override
    public void whereUsed(WhereUsedRequest request,
                          StreamObserver<WhereUsedResponse> responseObserver) {
        try {
            List<Bom> parents = bomRepository.findByChildItemCode(request.getItemCode());

            WhereUsedResponse.Builder response = WhereUsedResponse.newBuilder();
            for (Bom bom : parents) {
                response.addResults(WhereUsedResult.newBuilder()
                    .setParentItemCode(mapper.nullToEmpty(bom.getParentItemCode()))
                    .setChildItemCode(mapper.nullToEmpty(bom.getChildItemCode()))
                    .setRequiredQuantity(mapper.toProtoDecimal(bom.getRequiredQuantity()))
                    .build());
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
     * 子品目一覧取得
     */
    @Override
    public void getChildren(GetChildrenRequest request,
                            StreamObserver<GetChildrenResponse> responseObserver) {
        try {
            List<Bom> children = bomRepository.findByParentItemCode(request.getParentItemCode());

            GetChildrenResponse.Builder response = GetChildrenResponse.newBuilder();
            for (Bom bom : children) {
                response.addChildren(toProtoBom(bom));
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
     * BomExplosion -> Proto BomNode 変換
     */
    private BomNode toBomNode(BomExplosion explosion) {
        return BomNode.newBuilder()
            .setParentItemCode(mapper.nullToEmpty(explosion.getParentItemCode()))
            .setChildItemCode(mapper.nullToEmpty(explosion.getChildItemCode()))
            .setRequiredQuantity(mapper.toProtoDecimal(explosion.getRequiredQuantity()))
            .setTotalQuantity(mapper.toProtoDecimal(explosion.getTotalQuantity()))
            .setLevel(explosion.getLevel() != null ? explosion.getLevel() : 0)
            .setSequence(explosion.getSequence() != null ? explosion.getSequence() : 0)
            .build();
    }

    /**
     * Bom -> Proto Bom 変換
     */
    private com.example.production.grpc.Bom toProtoBom(Bom domain) {
        return com.example.production.grpc.Bom.newBuilder()
            .setParentItemCode(mapper.nullToEmpty(domain.getParentItemCode()))
            .setChildItemCode(mapper.nullToEmpty(domain.getChildItemCode()))
            .setEffectiveFrom(mapper.toProtoDate(domain.getEffectiveFrom()))
            .setEffectiveTo(mapper.toProtoDate(domain.getEffectiveTo()))
            .setBaseQuantity(mapper.toProtoDecimal(domain.getBaseQuantity()))
            .setRequiredQuantity(mapper.toProtoDecimal(domain.getRequiredQuantity()))
            .setDefectRate(mapper.toProtoDecimal(domain.getDefectRate()))
            .setSequence(domain.getSequence() != null ? domain.getSequence() : 0)
            .build();
    }
}
