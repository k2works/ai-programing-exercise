package com.example.production.application.service;

import com.example.production.application.port.in.command.*;
import com.example.production.application.port.out.*;
import com.example.production.domain.model.inventory.*;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.Year;
import java.util.List;
import java.util.UUID;
import java.util.stream.IntStream;

/**
 * 棚卸サービス
 */
@Service
@RequiredArgsConstructor
public class StocktakingService {

    private final StocktakingRepository stocktakingRepository;
    private final StockRepository stockRepository;
    private final StockAdjustmentRepository stockAdjustmentRepository;

    /**
     * 棚卸表を発行する
     */
    @Transactional
    public Stocktaking issueStocktakingSheet(StocktakingIssueCommand command) {
        List<Stock> stocks = stockRepository.findByLocationCode(command.getLocationCode());

        String stocktakingNumber = generateStocktakingNumber();

        Stocktaking stocktaking = Stocktaking.builder()
                .stocktakingNumber(stocktakingNumber)
                .locationCode(command.getLocationCode())
                .stocktakingDate(command.getStocktakingDate())
                .status(StocktakingStatus.ISSUED)
                .build();

        stocktakingRepository.save(stocktaking);

        List<StocktakingDetail> details = IntStream.range(0, stocks.size())
                .mapToObj(i -> {
                    Stock stock = stocks.get(i);
                    return StocktakingDetail.builder()
                            .stocktakingNumber(stocktakingNumber)
                            .lineNumber(i + 1)
                            .itemCode(stock.getItemCode())
                            .bookQuantity(stock.getStockQuantity())
                            .build();
                })
                .toList();

        details.forEach(stocktakingRepository::saveDetail);

        stocktaking.setDetails(details);
        return stocktaking;
    }

    /**
     * 実棚数量を入力する
     */
    @Transactional
    public Stocktaking inputActualCount(ActualCountInputCommand command) {
        Stocktaking stocktaking = stocktakingRepository.findByStocktakingNumber(command.getStocktakingNumber())
                .orElseThrow(() -> new IllegalArgumentException("棚卸データが見つかりません: " + command.getStocktakingNumber()));

        List<StocktakingDetail> existingDetails = stocktakingRepository.findDetailsByStocktakingNumber(
                command.getStocktakingNumber());

        for (ActualCountDetailCommand inputDetail : command.getDetails()) {
            existingDetails.stream()
                    .filter(d -> d.getItemCode().equals(inputDetail.getItemCode()))
                    .findFirst()
                    .ifPresent(detail -> {
                        BigDecimal difference = inputDetail.getActualQuantity()
                                .subtract(detail.getBookQuantity());

                        stocktakingRepository.updateDetail(
                                detail.getId(),
                                inputDetail.getActualQuantity(),
                                difference
                        );
                    });
        }

        stocktakingRepository.updateStatus(command.getStocktakingNumber(), StocktakingStatus.ENTERED);

        return stocktakingRepository.findByStocktakingNumberWithDetails(command.getStocktakingNumber());
    }

    /**
     * 棚卸を確定する
     */
    @Transactional
    public Stocktaking confirmStocktaking(StocktakingConfirmCommand command) {
        Stocktaking stocktaking = stocktakingRepository.findByStocktakingNumberWithDetails(
                command.getStocktakingNumber());

        if (stocktaking == null) {
            throw new IllegalArgumentException("棚卸データが見つかりません: " + command.getStocktakingNumber());
        }

        // 差異がある明細について在庫調整
        for (StocktakingDetail detail : stocktaking.getDetails()) {
            BigDecimal difference = detail.getDifferenceQuantity();

            if (difference != null && difference.compareTo(BigDecimal.ZERO) != 0) {
                // 在庫を調整
                Stock stock = stockRepository.findByLocationAndItem(
                        stocktaking.getLocationCode(), detail.getItemCode())
                        .orElseThrow(() -> new IllegalArgumentException(
                                "在庫が見つかりません: " + stocktaking.getLocationCode() + "/" + detail.getItemCode()));

                stock.setStockQuantity(stock.getStockQuantity().add(difference));
                stock.setPassedQuantity(stock.getPassedQuantity().add(difference));
                stockRepository.update(stock);

                // 在庫調整履歴を記録
                StockAdjustment adjustment = StockAdjustment.builder()
                        .adjustmentNumber(generateAdjustmentNumber())
                        .stocktakingNumber(command.getStocktakingNumber())
                        .itemCode(detail.getItemCode())
                        .locationCode(stocktaking.getLocationCode())
                        .adjustmentDate(LocalDate.now())
                        .adjusterCode(command.getAdjusterCode())
                        .adjustmentQuantity(difference)
                        .reasonCode(command.getAdjustmentReasonCode())
                        .build();

                stockAdjustmentRepository.save(adjustment);
            }
        }

        // 棚卸を確定
        stocktakingRepository.updateStatus(command.getStocktakingNumber(), StocktakingStatus.CONFIRMED);

        return stocktakingRepository.findByStocktakingNumberWithDetails(command.getStocktakingNumber());
    }

    /**
     * 棚卸データを取得する
     */
    public Stocktaking findByStocktakingNumber(String stocktakingNumber) {
        return stocktakingRepository.findByStocktakingNumberWithDetails(stocktakingNumber);
    }

    /**
     * 在庫調整履歴を取得する
     */
    public List<StockAdjustment> findAdjustmentsByStocktakingNumber(String stocktakingNumber) {
        return stockAdjustmentRepository.findByStocktakingNumber(stocktakingNumber);
    }

    private String generateStocktakingNumber() {
        int year = Year.now().getValue();
        long count = stocktakingRepository.countByYear(year);
        return String.format("ST-%d-%04d", year, count + 1);
    }

    private String generateAdjustmentNumber() {
        return "ADJ-" + UUID.randomUUID().toString().substring(0, 8).toUpperCase();
    }
}
