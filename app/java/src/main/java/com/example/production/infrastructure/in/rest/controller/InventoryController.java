package com.example.production.infrastructure.in.rest.controller;

import com.example.production.application.port.in.InventoryUseCase;
import com.example.production.domain.model.inventory.Stock;
import com.example.production.infrastructure.in.rest.dto.StockResponse;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

/**
 * 在庫 Controller（Input Adapter）
 */
@RestController
@RequestMapping("/api/inventory")
@Tag(name = "inventory", description = "在庫 API")
@RequiredArgsConstructor
public class InventoryController {

    private final InventoryUseCase useCase;

    @GetMapping
    @Operation(summary = "在庫一覧の取得")
    public ResponseEntity<List<StockResponse>> getInventory(
            @Parameter(description = "品目コード")
            @RequestParam(required = false) String itemCode,
            @Parameter(description = "場所コード")
            @RequestParam(required = false) String locationCode,
            @Parameter(description = "在庫状態")
            @RequestParam(required = false) String status) {

        InventoryUseCase.InventoryQuery query = new InventoryUseCase.InventoryQuery(
                itemCode, locationCode, status);

        List<Stock> stocks = useCase.getInventory(query);
        return ResponseEntity.ok(stocks.stream()
                .map(StockResponse::from)
                .toList());
    }

    @GetMapping("/location/{locationCode}")
    @Operation(summary = "場所別在庫一覧の取得")
    public ResponseEntity<List<StockResponse>> getStocksByLocation(
            @Parameter(description = "場所コード", required = true)
            @PathVariable String locationCode) {

        List<Stock> stocks = useCase.getStocksByLocation(locationCode);
        return ResponseEntity.ok(stocks.stream()
                .map(StockResponse::from)
                .toList());
    }

    @GetMapping("/item/{itemCode}")
    @Operation(summary = "品目別在庫一覧の取得")
    public ResponseEntity<List<StockResponse>> getStocksByItem(
            @Parameter(description = "品目コード", required = true)
            @PathVariable String itemCode) {

        List<Stock> stocks = useCase.getStocksByItem(itemCode);
        return ResponseEntity.ok(stocks.stream()
                .map(StockResponse::from)
                .toList());
    }

    @GetMapping("/{locationCode}/{itemCode}")
    @Operation(summary = "在庫の取得")
    public ResponseEntity<StockResponse> getStock(
            @Parameter(description = "場所コード", required = true)
            @PathVariable String locationCode,
            @Parameter(description = "品目コード", required = true)
            @PathVariable String itemCode) {

        Stock stock = useCase.getStock(locationCode, itemCode);
        return ResponseEntity.ok(StockResponse.from(stock));
    }
}
