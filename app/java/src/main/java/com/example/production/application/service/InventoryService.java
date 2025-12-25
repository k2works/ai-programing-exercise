package com.example.production.application.service;

import com.example.production.application.port.in.command.StockChangeCommand;
import com.example.production.application.port.in.command.StockStatusChangeCommand;
import com.example.production.application.port.out.StockRepository;
import com.example.production.domain.model.inventory.InsufficientStockException;
import com.example.production.domain.model.inventory.Stock;
import com.example.production.domain.model.inventory.StockStatus;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.util.List;

/**
 * 在庫管理サービス
 */
@Service
@RequiredArgsConstructor
public class InventoryService {

    private final StockRepository stockRepository;

    /**
     * 在庫を取得する
     */
    public Stock getStock(String locationCode, String itemCode) {
        return stockRepository.findByLocationAndItem(locationCode, itemCode)
                .orElse(Stock.empty(locationCode, itemCode));
    }

    /**
     * 場所別の在庫一覧を取得する
     */
    public List<Stock> getStocksByLocation(String locationCode) {
        return stockRepository.findByLocationCode(locationCode);
    }

    /**
     * 品目別の在庫一覧を取得する
     */
    public List<Stock> getStocksByItem(String itemCode) {
        return stockRepository.findByItemCode(itemCode);
    }

    /**
     * 在庫を増加する
     */
    @Transactional
    public void increaseStock(StockChangeCommand command) {
        Stock existingStock = stockRepository.findByLocationAndItem(command.getLocationCode(), command.getItemCode())
                .orElse(null);

        if (existingStock == null) {
            // 新規作成
            Stock newStock = Stock.builder()
                    .locationCode(command.getLocationCode())
                    .itemCode(command.getItemCode())
                    .stockQuantity(command.getQuantity())
                    .passedQuantity(command.getStockStatus() == StockStatus.PASSED ? command.getQuantity() : BigDecimal.ZERO)
                    .defectiveQuantity(command.getStockStatus() == StockStatus.DEFECTIVE ? command.getQuantity() : BigDecimal.ZERO)
                    .uninspectedQuantity(command.getStockStatus() == StockStatus.UNINSPECTED ? command.getQuantity() : BigDecimal.ZERO)
                    .build();
            stockRepository.save(newStock);
        } else {
            // 更新
            stockRepository.increaseByStatus(
                    command.getLocationCode(),
                    command.getItemCode(),
                    command.getQuantity(),
                    command.getStockStatus()
            );
        }
    }

    /**
     * 在庫を減少する
     */
    @Transactional
    public void decreaseStock(StockChangeCommand command) {
        Stock stock = getStock(command.getLocationCode(), command.getItemCode());
        BigDecimal currentQuantity = stock.getQuantityByStatus(command.getStockStatus());

        if (currentQuantity.compareTo(command.getQuantity()) < 0) {
            throw new InsufficientStockException(
                    String.format("在庫が不足しています。場所: %s, 品目: %s, 要求数: %s, 現在数: %s",
                            command.getLocationCode(), command.getItemCode(),
                            command.getQuantity(), currentQuantity)
            );
        }

        stockRepository.decreaseByStatus(
                command.getLocationCode(),
                command.getItemCode(),
                command.getQuantity(),
                command.getStockStatus()
        );
    }

    /**
     * 在庫状態を変更する
     */
    @Transactional
    public void changeStockStatus(StockStatusChangeCommand command) {
        Stock stock = getStock(command.getLocationCode(), command.getItemCode());
        BigDecimal currentQuantity = stock.getQuantityByStatus(command.getFromStatus());

        if (currentQuantity.compareTo(command.getQuantity()) < 0) {
            throw new InsufficientStockException(
                    String.format("%sの在庫が不足しています。場所: %s, 品目: %s, 要求数: %s, 現在数: %s",
                            command.getFromStatus().getDisplayName(),
                            command.getLocationCode(), command.getItemCode(),
                            command.getQuantity(), currentQuantity)
            );
        }

        stockRepository.changeStatus(
                command.getLocationCode(),
                command.getItemCode(),
                command.getQuantity(),
                command.getFromStatus(),
                command.getToStatus()
        );
    }
}
