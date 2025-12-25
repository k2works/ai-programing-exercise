package com.example.production.application.service;

import com.example.production.application.port.in.command.ConsumptionCreateCommand;
import com.example.production.application.port.in.command.ConsumptionDetailCommand;
import com.example.production.application.port.out.*;
import com.example.production.domain.model.purchase.Receiving;
import com.example.production.domain.model.subcontract.*;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

/**
 * 消費サービス
 */
@Service
@RequiredArgsConstructor
public class ConsumptionService {

    private final ConsumptionRepository consumptionRepository;
    private final ConsumptionDetailRepository consumptionDetailRepository;
    private final ReceivingRepository receivingRepository;
    private final SupplyRepository supplyRepository;
    private final SupplyDetailRepository supplyDetailRepository;

    /**
     * 消費番号を生成する
     */
    private String generateConsumptionNumber(LocalDate consumptionDate) {
        String prefix = "CON-" + consumptionDate.format(DateTimeFormatter.ofPattern("yyyyMM")) + "-";
        Optional<String> latestNumber = consumptionRepository.findLatestConsumptionNumber(prefix);

        int sequence = 1;
        if (latestNumber.isPresent()) {
            String latest = latestNumber.get();
            int currentSequence = Integer.parseInt(latest.substring(latest.length() - 4));
            sequence = currentSequence + 1;
        }

        return prefix + String.format("%04d", sequence);
    }

    /**
     * 消費データを作成する
     */
    @Transactional
    public Consumption createConsumption(ConsumptionCreateCommand command) {
        // 入荷データの取得
        Receiving receiving = receivingRepository.findByReceivingNumber(command.getReceivingNumber())
                .orElseThrow(() -> new IllegalArgumentException("Receiving not found: " + command.getReceivingNumber()));

        // 関連する支給データを取得
        List<Supply> supplies = supplyRepository.findByPurchaseOrderDetail(
                receiving.getPurchaseOrderNumber(), receiving.getLineNumber());
        if (supplies.isEmpty()) {
            throw new IllegalArgumentException("Supply not found for receiving: " + command.getReceivingNumber());
        }

        // 最初の支給を使用（通常は1つ）
        Supply supply = supplies.get(0);
        List<SupplyDetail> supplyDetails = supplyDetailRepository.findBySupplyNumber(supply.getSupplyNumber());

        // 消費数量のバリデーション
        for (ConsumptionDetailCommand detailCommand : command.getDetails()) {
            for (SupplyDetail supplyDetail : supplyDetails) {
                if (supplyDetail.getItemCode().equals(detailCommand.getItemCode())) {
                    if (detailCommand.getQuantity().compareTo(supplyDetail.getQuantity()) > 0) {
                        throw new IllegalStateException("Consumption quantity exceeds supply quantity");
                    }
                }
            }
        }

        String consumptionNumber = generateConsumptionNumber(command.getConsumptionDate());

        // 消費ヘッダを作成
        Consumption consumption = Consumption.builder()
                .consumptionNumber(consumptionNumber)
                .receivingNumber(command.getReceivingNumber())
                .consumptionDate(command.getConsumptionDate())
                .supplierCode(command.getSupplierCode())
                .remarks(command.getRemarks())
                .build();
        consumptionRepository.save(consumption);

        // 消費明細を作成
        List<ConsumptionDetail> details = new ArrayList<>();
        int lineNumber = 0;

        for (ConsumptionDetailCommand detailCommand : command.getDetails()) {
            lineNumber++;

            ConsumptionDetail detail = ConsumptionDetail.builder()
                    .consumptionNumber(consumptionNumber)
                    .lineNumber(lineNumber)
                    .itemCode(detailCommand.getItemCode())
                    .quantity(detailCommand.getQuantity())
                    .remarks(detailCommand.getRemarks())
                    .build();
            consumptionDetailRepository.save(detail);

            details.add(detail);
        }

        consumption.setDetails(details);
        return consumption;
    }

    /**
     * 消費番号で検索する
     */
    public Optional<Consumption> findByConsumptionNumber(String consumptionNumber) {
        Optional<Consumption> consumption = consumptionRepository.findByConsumptionNumber(consumptionNumber);
        consumption.ifPresent(c -> c.setDetails(consumptionDetailRepository.findByConsumptionNumber(consumptionNumber)));
        return consumption;
    }

    /**
     * 消費率を計算する
     */
    public BigDecimal calculateConsumptionRate(String supplyNumber, String itemCode) {
        Supply supply = supplyRepository.findBySupplyNumber(supplyNumber)
                .orElseThrow(() -> new IllegalArgumentException("Supply not found: " + supplyNumber));

        List<SupplyDetail> supplyDetails = supplyDetailRepository.findBySupplyNumber(supplyNumber);
        SupplyDetail targetDetail = supplyDetails.stream()
                .filter(d -> d.getItemCode().equals(itemCode))
                .findFirst()
                .orElseThrow(() -> new IllegalArgumentException("Supply detail not found: " + itemCode));

        BigDecimal supplyQuantity = targetDetail.getQuantity();

        BigDecimal totalConsumption = consumptionDetailRepository.sumByPurchaseOrderAndItem(
                supply.getPurchaseOrderNumber(),
                supply.getLineNumber(),
                itemCode
        );

        if (totalConsumption == null) {
            totalConsumption = BigDecimal.ZERO;
        }

        return totalConsumption.divide(supplyQuantity, 2, RoundingMode.HALF_UP);
    }
}
