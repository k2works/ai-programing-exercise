package com.example.production.infrastructure.web.controller;

import com.example.production.application.port.in.ItemUseCase;
import com.example.production.application.port.in.command.CreateItemCommand;
import com.example.production.application.port.in.command.UpdateItemCommand;
import com.example.production.domain.model.item.Item;
import com.example.production.domain.model.item.ItemCategory;
import com.example.production.infrastructure.web.dto.CreateItemRequest;
import com.example.production.infrastructure.web.dto.ItemResponse;
import com.example.production.infrastructure.web.dto.UpdateItemRequest;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

/**
 * 品目 Controller（Input Adapter）
 */
@RestController
@RequestMapping("/api/items")
@Tag(name = "items", description = "品目マスタ API")
@RequiredArgsConstructor
public class ItemController {

    private final ItemUseCase itemUseCase;

    @GetMapping
    @Operation(
            summary = "品目一覧の取得",
            description = "すべての品目を取得します。category クエリパラメータでフィルタリング可能です。"
    )
    public ResponseEntity<List<ItemResponse>> getAllItems(
            @Parameter(description = "品目区分でフィルタリング")
            @RequestParam(required = false) ItemCategory category) {

        List<Item> items;
        if (category != null) {
            items = itemUseCase.getItemsByCategory(category);
        } else {
            items = itemUseCase.getAllItems();
        }

        return ResponseEntity.ok(items.stream()
                .map(ItemResponse::from)
                .toList());
    }

    @GetMapping("/category/{category}")
    @Operation(
            summary = "カテゴリ別品目一覧の取得",
            description = "指定したカテゴリの品目を取得します"
    )
    public ResponseEntity<List<ItemResponse>> getItemsByCategory(
            @Parameter(description = "品目区分", required = true)
            @PathVariable ItemCategory category) {

        List<Item> items = itemUseCase.getItemsByCategory(category);
        return ResponseEntity.ok(items.stream()
                .map(ItemResponse::from)
                .toList());
    }

    @GetMapping("/{itemCode}")
    @Operation(summary = "品目の取得")
    @ApiResponse(responseCode = "200", description = "成功")
    @ApiResponse(responseCode = "404", description = "品目が見つからない")
    public ResponseEntity<ItemResponse> getItemByCode(
            @Parameter(description = "品目コード", required = true)
            @PathVariable String itemCode) {

        Item item = itemUseCase.getItemByCode(itemCode);
        return ResponseEntity.ok(ItemResponse.from(item));
    }

    @PostMapping
    @Operation(summary = "品目の登録")
    @ApiResponse(responseCode = "201", description = "登録成功")
    @ApiResponse(responseCode = "409", description = "品目コード重複")
    public ResponseEntity<ItemResponse> createItem(
            @Valid @RequestBody CreateItemRequest request) {

        CreateItemCommand command = CreateItemCommand.builder()
                .itemCode(request.getItemCode())
                .itemName(request.getItemName())
                .category(request.getCategory())
                .unitCode(request.getUnitCode())
                .leadTime(request.getLeadTime())
                .safetyLeadTime(request.getSafetyLeadTime())
                .safetyStock(request.getSafetyStock())
                .yieldRate(request.getYieldRate())
                .minLotSize(request.getMinLotSize())
                .lotIncrement(request.getLotIncrement())
                .maxLotSize(request.getMaxLotSize())
                .shelfLife(request.getShelfLife())
                .build();

        Item item = itemUseCase.createItem(command);
        return ResponseEntity.status(HttpStatus.CREATED)
                .body(ItemResponse.from(item));
    }

    @PutMapping("/{itemCode}")
    @Operation(summary = "品目の更新")
    public ResponseEntity<ItemResponse> updateItem(
            @PathVariable String itemCode,
            @Valid @RequestBody UpdateItemRequest request) {

        UpdateItemCommand command = UpdateItemCommand.builder()
                .itemCode(itemCode)
                .itemName(request.getItemName())
                .category(request.getCategory())
                .leadTime(request.getLeadTime())
                .safetyLeadTime(request.getSafetyLeadTime())
                .safetyStock(request.getSafetyStock())
                .yieldRate(request.getYieldRate())
                .minLotSize(request.getMinLotSize())
                .lotIncrement(request.getLotIncrement())
                .maxLotSize(request.getMaxLotSize())
                .shelfLife(request.getShelfLife())
                .build();

        Item item = itemUseCase.updateItem(command);
        return ResponseEntity.ok(ItemResponse.from(item));
    }

    @DeleteMapping("/{itemCode}")
    @Operation(summary = "品目の削除")
    @ResponseStatus(HttpStatus.NO_CONTENT)
    public void deleteItem(@PathVariable String itemCode) {
        itemUseCase.deleteItem(itemCode);
    }
}
