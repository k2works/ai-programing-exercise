package com.example.production.application.port.in;

import com.example.production.domain.model.inventory.Stock;

import java.util.List;

/**
 * 在庫ユースケース（Input Port）
 */
public interface InventoryUseCase {

    /**
     * 在庫一覧を取得
     */
    List<Stock> getInventory(InventoryQuery query);

    /**
     * 場所と品目で在庫を取得
     */
    Stock getStock(String locationCode, String itemCode);

    /**
     * 場所別の在庫一覧を取得
     */
    List<Stock> getStocksByLocation(String locationCode);

    /**
     * 品目別の在庫一覧を取得
     */
    List<Stock> getStocksByItem(String itemCode);

    /**
     * すべての在庫を取得
     */
    List<Stock> getAllStocks();

    /**
     * 在庫照会クエリ
     */
    record InventoryQuery(
            String itemCode,
            String locationCode,
            String stockStatus
    ) {}
}
