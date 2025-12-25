package com.example.production.application.service;

import com.example.production.application.port.in.PurchaseOrderUseCase;
import com.example.production.application.port.in.command.CreatePurchaseOrderCommand;
import com.example.production.application.port.out.PurchaseOrderRepository;
import com.example.production.application.port.out.PurchaseOrderDetailRepository;
import com.example.production.domain.exception.PurchaseOrderNotFoundException;
import com.example.production.domain.model.purchase.PurchaseOrder;
import com.example.production.domain.model.purchase.PurchaseOrderDetail;
import com.example.production.domain.model.purchase.PurchaseOrderStatus;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.List;

/**
 * 発注サービス
 */
@Service
@RequiredArgsConstructor
public class PurchaseOrderService implements PurchaseOrderUseCase {

    private final PurchaseOrderRepository purchaseOrderRepository;
    private final PurchaseOrderDetailRepository purchaseOrderDetailRepository;

    /**
     * 発注番号を生成する
     */
    private String generatePurchaseOrderNumber(LocalDate orderDate) {
        String prefix = "PO-" + orderDate.format(DateTimeFormatter.ofPattern("yyyyMM")) + "-";
        return purchaseOrderRepository.findLatestPurchaseOrderNumber(prefix)
                .map(latest -> {
                    int currentSequence = Integer.parseInt(latest.substring(latest.length() - 4));
                    return prefix + String.format("%04d", currentSequence + 1);
                })
                .orElse(prefix + "0001");
    }

    @Override
    @Transactional
    public PurchaseOrder createOrder(CreatePurchaseOrderCommand command) {
        LocalDate orderDate = LocalDate.now();
        String purchaseOrderNumber = generatePurchaseOrderNumber(orderDate);

        PurchaseOrder purchaseOrder = PurchaseOrder.builder()
                .purchaseOrderNumber(purchaseOrderNumber)
                .orderDate(orderDate)
                .supplierCode(command.getSupplierCode())
                .ordererCode(command.getOrdererCode())
                .departmentCode(command.getDepartmentCode())
                .status(PurchaseOrderStatus.CREATING)
                .remarks(command.getRemarks())
                .build();
        purchaseOrderRepository.save(purchaseOrder);

        List<PurchaseOrderDetail> details = new ArrayList<>();
        if (command.getDetails() != null) {
            int lineNumber = 1;
            for (var detailCommand : command.getDetails()) {
                // 発注金額を計算
                BigDecimal orderAmount = detailCommand.getUnitPrice() != null && detailCommand.getOrderQuantity() != null
                        ? detailCommand.getUnitPrice().multiply(detailCommand.getOrderQuantity())
                        : BigDecimal.ZERO;

                PurchaseOrderDetail detail = PurchaseOrderDetail.builder()
                        .purchaseOrderNumber(purchaseOrderNumber)
                        .lineNumber(lineNumber++)
                        .itemCode(detailCommand.getItemCode())
                        .orderQuantity(detailCommand.getOrderQuantity())
                        .orderUnitPrice(detailCommand.getUnitPrice())
                        .orderAmount(orderAmount)
                        .expectedReceivingDate(detailCommand.getDeliveryDate())
                        .build();
                purchaseOrderDetailRepository.save(detail);
                details.add(detail);
            }
        }

        purchaseOrder.setDetails(details);
        return purchaseOrder;
    }

    @Override
    @Transactional(readOnly = true)
    public PurchaseOrder getOrder(String orderNumber) {
        PurchaseOrder purchaseOrder = purchaseOrderRepository.findByPurchaseOrderNumber(orderNumber)
                .orElseThrow(() -> new PurchaseOrderNotFoundException(orderNumber));
        purchaseOrder.setDetails(purchaseOrderDetailRepository.findByPurchaseOrderNumber(orderNumber));
        return purchaseOrder;
    }

    @Override
    @Transactional(readOnly = true)
    public List<PurchaseOrder> getAllOrders() {
        return purchaseOrderRepository.findAll();
    }

    @Override
    @Transactional
    public PurchaseOrder confirmOrder(String orderNumber) {
        PurchaseOrder purchaseOrder = purchaseOrderRepository.findByPurchaseOrderNumber(orderNumber)
                .orElseThrow(() -> new PurchaseOrderNotFoundException(orderNumber));

        if (purchaseOrder.getStatus() != PurchaseOrderStatus.CREATING) {
            throw new IllegalStateException("Only CREATING orders can be confirmed");
        }

        purchaseOrderRepository.updateStatus(orderNumber, PurchaseOrderStatus.ORDERED);
        return getOrder(orderNumber);
    }

    @Override
    @Transactional
    public void cancelOrder(String orderNumber) {
        PurchaseOrder purchaseOrder = purchaseOrderRepository.findByPurchaseOrderNumber(orderNumber)
                .orElseThrow(() -> new PurchaseOrderNotFoundException(orderNumber));

        if (purchaseOrder.getStatus() != PurchaseOrderStatus.CREATING &&
            purchaseOrder.getStatus() != PurchaseOrderStatus.ORDERED) {
            throw new IllegalStateException("Only CREATING or ORDERED orders can be cancelled");
        }

        purchaseOrderRepository.updateStatus(orderNumber, PurchaseOrderStatus.CANCELLED);
    }
}
