package com.example.production.application.service;

import com.example.production.application.port.out.SupplyDetailRepository;
import com.example.production.application.port.out.SupplyRepository;
import com.example.production.domain.model.subcontract.Supply;
import com.example.production.domain.model.subcontract.SupplyDetail;
import com.example.production.domain.model.subcontract.SupplyType;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

/**
 * 支給サービス
 */
@Service
@RequiredArgsConstructor
public class SupplyService {

    private final SupplyRepository supplyRepository;
    private final SupplyDetailRepository supplyDetailRepository;

    /**
     * 支給番号を生成する
     */
    private String generateSupplyNumber(LocalDate supplyDate) {
        String prefix = "SUP-" + supplyDate.format(DateTimeFormatter.ofPattern("yyyyMM")) + "-";
        Optional<String> latestNumber = supplyRepository.findLatestSupplyNumber(prefix);

        int sequence = 1;
        if (latestNumber.isPresent()) {
            String latest = latestNumber.get();
            int currentSequence = Integer.parseInt(latest.substring(latest.length() - 4));
            sequence = currentSequence + 1;
        }

        return prefix + String.format("%04d", sequence);
    }

    /**
     * 支給データを作成する
     */
    @Transactional
    public Supply createSupply(SupplyCreateInput input) {
        String supplyNumber = generateSupplyNumber(input.getSupplyDate());
        SupplyType supplyType = input.getSupplyType() != null ? input.getSupplyType() : SupplyType.FREE;

        // 支給ヘッダを作成
        Supply supply = Supply.builder()
                .supplyNumber(supplyNumber)
                .purchaseOrderNumber(input.getPurchaseOrderNumber())
                .lineNumber(input.getLineNumber())
                .supplierCode(input.getSupplierCode())
                .supplyDate(input.getSupplyDate())
                .supplierPersonCode(input.getSupplierPersonCode())
                .supplyType(supplyType)
                .remarks(input.getRemarks())
                .build();
        supplyRepository.save(supply);

        // 支給明細を作成
        List<SupplyDetail> details = new ArrayList<>();
        int detailLineNumber = 0;

        for (SupplyDetailInput detailInput : input.getDetails()) {
            detailLineNumber++;

            SupplyDetail detail = SupplyDetail.builder()
                    .supplyNumber(supplyNumber)
                    .lineNumber(detailLineNumber)
                    .itemCode(detailInput.getItemCode())
                    .quantity(detailInput.getQuantity())
                    .unitPrice(detailInput.getUnitPrice())
                    .amount(detailInput.getQuantity().multiply(detailInput.getUnitPrice()))
                    .remarks(detailInput.getRemarks())
                    .build();
            supplyDetailRepository.save(detail);

            details.add(detail);
        }

        supply.setDetails(details);
        return supply;
    }

    /**
     * 支給番号で検索する
     */
    public Optional<Supply> findBySupplyNumber(String supplyNumber) {
        Optional<Supply> supply = supplyRepository.findBySupplyNumber(supplyNumber);
        supply.ifPresent(s -> s.setDetails(supplyDetailRepository.findBySupplyNumber(supplyNumber)));
        return supply;
    }

    /**
     * 発注明細で検索する
     */
    public List<Supply> findByPurchaseOrderDetail(String purchaseOrderNumber, Integer lineNumber) {
        List<Supply> supplies = supplyRepository.findByPurchaseOrderDetail(purchaseOrderNumber, lineNumber);
        for (Supply supply : supplies) {
            supply.setDetails(supplyDetailRepository.findBySupplyNumber(supply.getSupplyNumber()));
        }
        return supplies;
    }
}
