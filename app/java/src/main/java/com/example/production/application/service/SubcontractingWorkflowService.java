package com.example.production.application.service;

import com.example.production.application.port.in.command.SubcontractOrderCommand;
import com.example.production.application.port.out.*;
import com.example.production.domain.model.purchase.*;
import com.example.production.infrastructure.dto.SubcontractStatus;
import com.example.production.domain.model.subcontract.*;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.List;
import java.util.Optional;

/**
 * 外注委託ワークフローサービス
 */
@Service
@RequiredArgsConstructor
public class SubcontractingWorkflowService {

    private final PurchaseOrderRepository purchaseOrderRepository;
    private final PurchaseOrderDetailRepository purchaseOrderDetailRepository;
    private final SupplyRepository supplyRepository;
    private final SupplyDetailRepository supplyDetailRepository;
    private final ConsumptionDetailRepository consumptionDetailRepository;
    private final UnitPriceRepository unitPriceRepository;

    /**
     * 発注番号を生成する
     */
    private String generatePurchaseOrderNumber() {
        String prefix = "PO-" + LocalDate.now().format(DateTimeFormatter.ofPattern("yyyyMM")) + "-";
        Optional<String> latestNumber = purchaseOrderRepository.findLatestPurchaseOrderNumber(prefix);

        int sequence = 1;
        if (latestNumber.isPresent()) {
            String latest = latestNumber.get();
            int currentSequence = Integer.parseInt(latest.substring(latest.length() - 4));
            sequence = currentSequence + 1;
        }

        return prefix + String.format("%04d", sequence);
    }

    /**
     * 外注発注を作成する
     */
    @Transactional
    public PurchaseOrder createSubcontractOrder(SubcontractOrderCommand command) {
        String purchaseOrderNumber = generatePurchaseOrderNumber();

        // 単価を取得
        BigDecimal unitPrice = command.getUnitPrice();
        if (unitPrice == null) {
            Optional<UnitPrice> priceOpt = unitPriceRepository.findEffectiveUnitPrice(
                    command.getItemCode(), command.getSupplierCode(), LocalDate.now());
            unitPrice = priceOpt.map(UnitPrice::getUnitPrice).orElse(BigDecimal.ZERO);
        }

        // 発注ヘッダを作成
        PurchaseOrder purchaseOrder = PurchaseOrder.builder()
                .purchaseOrderNumber(purchaseOrderNumber)
                .orderDate(LocalDate.now())
                .supplierCode(command.getSupplierCode())
                .status(PurchaseOrderStatus.CREATING)
                .build();
        purchaseOrderRepository.save(purchaseOrder);

        // 発注明細を作成
        BigDecimal orderAmount = command.getQuantity().multiply(unitPrice);
        PurchaseOrderDetail detail = PurchaseOrderDetail.builder()
                .purchaseOrderNumber(purchaseOrderNumber)
                .lineNumber(1)
                .itemCode(command.getItemCode())
                .orderQuantity(command.getQuantity())
                .orderUnitPrice(unitPrice)
                .orderAmount(orderAmount)
                .expectedReceivingDate(command.getDeliveryDate())
                .completedFlag(false)
                .build();
        purchaseOrderDetailRepository.save(detail);

        // 発注確定
        purchaseOrderRepository.updateStatus(purchaseOrderNumber, PurchaseOrderStatus.ORDERED);
        purchaseOrder.setStatus(PurchaseOrderStatus.ORDERED);

        return purchaseOrder;
    }

    /**
     * 外注委託状況を取得する
     */
    public SubcontractStatus getSubcontractStatus(String purchaseOrderNumber) {
        PurchaseOrder purchaseOrder = purchaseOrderRepository.findByPurchaseOrderNumber(purchaseOrderNumber)
                .orElseThrow(() -> new IllegalArgumentException("Purchase order not found: " + purchaseOrderNumber));

        List<PurchaseOrderDetail> details = purchaseOrderDetailRepository.findByPurchaseOrderNumber(purchaseOrderNumber);

        BigDecimal suppliedQuantity = BigDecimal.ZERO;
        BigDecimal consumedQuantity = BigDecimal.ZERO;
        BigDecimal acceptedQuantity = BigDecimal.ZERO;

        for (PurchaseOrderDetail detail : details) {
            List<Supply> supplies = supplyRepository.findByPurchaseOrderDetail(purchaseOrderNumber, detail.getLineNumber());
            for (Supply supply : supplies) {
                List<SupplyDetail> supplyDetails = supplyDetailRepository.findBySupplyNumber(supply.getSupplyNumber());
                for (SupplyDetail supplyDetail : supplyDetails) {
                    suppliedQuantity = suppliedQuantity.add(supplyDetail.getQuantity());

                    BigDecimal consumed = consumptionDetailRepository.sumByPurchaseOrderAndItem(
                            purchaseOrderNumber, detail.getLineNumber(), supplyDetail.getItemCode());
                    if (consumed != null) {
                        consumedQuantity = consumedQuantity.add(consumed);
                    }
                }
            }

            if (detail.getAcceptedQuantity() != null) {
                acceptedQuantity = acceptedQuantity.add(detail.getAcceptedQuantity());
            }
        }

        BigDecimal yieldRate = suppliedQuantity.compareTo(BigDecimal.ZERO) > 0
                ? consumedQuantity.divide(suppliedQuantity, 2, RoundingMode.HALF_UP)
                : BigDecimal.ZERO;

        return SubcontractStatus.builder()
                .purchaseOrderNumber(purchaseOrderNumber)
                .status(purchaseOrder.getStatus())
                .suppliedQuantity(suppliedQuantity)
                .consumedQuantity(consumedQuantity)
                .acceptedQuantity(acceptedQuantity)
                .yieldRate(yieldRate)
                .build();
    }
}
