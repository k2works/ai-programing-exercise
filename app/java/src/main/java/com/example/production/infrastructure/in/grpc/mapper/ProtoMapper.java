package com.example.production.infrastructure.in.grpc.mapper;

import com.example.production.application.port.in.command.CreateItemCommand;
import com.example.production.domain.model.item.Item;
import com.example.production.domain.model.item.ItemCategory;
import com.example.production.grpc.CreateItemRequest;
import com.example.production.grpc.Date;
import com.example.production.grpc.Decimal;
import org.springframework.stereotype.Component;

import java.math.BigDecimal;
import java.time.LocalDate;

/**
 * ドメインモデルと Protocol Buffers メッセージ間の変換
 */
@Component
public class ProtoMapper {

    // ========== Item ==========

    /**
     * ドメイン Item -> Proto Item
     */
    public com.example.production.grpc.Item toProto(Item domain) {
        return com.example.production.grpc.Item.newBuilder()
            .setItemCode(nullToEmpty(domain.getItemCode()))
            .setItemName(nullToEmpty(domain.getItemName()))
            .setCategory(toProtoCategory(domain.getItemCategory()))
            .setUnitCode(nullToEmpty(domain.getUnitCode()))
            .setLeadTime(nullToZero(domain.getLeadTime()))
            .setSafetyLeadTime(nullToZero(domain.getSafetyLeadTime()))
            .setSafetyStock(toProtoDecimal(domain.getSafetyStock()))
            .setYieldRate(toProtoDecimal(domain.getYieldRate()))
            .setMinLotSize(toProtoDecimal(domain.getMinLotSize()))
            .setLotIncrement(toProtoDecimal(domain.getLotIncrement()))
            .setMaxLotSize(toProtoDecimal(domain.getMaxLotSize()))
            .setShelfLife(nullToZero(domain.getShelfLife()))
            .build();
    }

    /**
     * Proto CreateItemRequest -> CreateItemCommand
     */
    public CreateItemCommand toCommand(CreateItemRequest request) {
        return CreateItemCommand.builder()
            .itemCode(request.getItemCode())
            .itemName(request.getItemName())
            .category(toDomainCategory(request.getCategory()))
            .unitCode(emptyToNull(request.getUnitCode()))
            .leadTime(zeroToNull(request.getLeadTime()))
            .safetyLeadTime(zeroToNull(request.getSafetyLeadTime()))
            .safetyStock(toDomainDecimalOrNull(request.getSafetyStock()))
            .yieldRate(toDomainDecimalOrNull(request.getYieldRate()))
            .minLotSize(toDomainDecimalOrNull(request.getMinLotSize()))
            .lotIncrement(toDomainDecimalOrNull(request.getLotIncrement()))
            .maxLotSize(toDomainDecimalOrNull(request.getMaxLotSize()))
            .shelfLife(zeroToNull(request.getShelfLife()))
            .build();
    }

    // ========== Date ==========

    /**
     * LocalDate -> Proto Date
     */
    public Date toProtoDate(LocalDate date) {
        if (date == null) {
            return Date.getDefaultInstance();
        }
        return Date.newBuilder()
            .setYear(date.getYear())
            .setMonth(date.getMonthValue())
            .setDay(date.getDayOfMonth())
            .build();
    }

    /**
     * Proto Date -> LocalDate
     */
    public LocalDate toDomainDate(Date date) {
        if (date == null || (date.getYear() == 0 && date.getMonth() == 0 && date.getDay() == 0)) {
            return null;
        }
        return LocalDate.of(date.getYear(), date.getMonth(), date.getDay());
    }

    // ========== Decimal ==========

    /**
     * BigDecimal -> Proto Decimal
     */
    public Decimal toProtoDecimal(BigDecimal value) {
        if (value == null) {
            return Decimal.getDefaultInstance();
        }
        return Decimal.newBuilder()
            .setValue(value.toPlainString())
            .setScale(value.scale())
            .build();
    }

    /**
     * Proto Decimal -> BigDecimal
     */
    public BigDecimal toDomainDecimal(Decimal decimal) {
        if (decimal == null || decimal.getValue().isEmpty()) {
            return BigDecimal.ZERO;
        }
        return new BigDecimal(decimal.getValue());
    }

    /**
     * Proto Decimal -> BigDecimal (空の場合は null)
     */
    public BigDecimal toDomainDecimalOrNull(Decimal decimal) {
        if (decimal == null || decimal.getValue().isEmpty()) {
            return null;
        }
        return new BigDecimal(decimal.getValue());
    }

    // ========== ItemCategory ==========

    /**
     * ドメインカテゴリ -> Proto カテゴリ
     */
    public com.example.production.grpc.ItemCategory toProtoCategory(ItemCategory domain) {
        if (domain == null) {
            return com.example.production.grpc.ItemCategory.ITEM_CATEGORY_UNSPECIFIED;
        }
        return switch (domain) {
            case PRODUCT -> com.example.production.grpc.ItemCategory.PRODUCT;
            case SEMI_PRODUCT -> com.example.production.grpc.ItemCategory.SEMI_PRODUCT;
            case PART -> com.example.production.grpc.ItemCategory.COMPONENT;
            case RAW_MATERIAL, MATERIAL -> com.example.production.grpc.ItemCategory.RAW_MATERIAL;
            case SUPPLY -> com.example.production.grpc.ItemCategory.PURCHASED_PART;
            case INTERMEDIATE -> com.example.production.grpc.ItemCategory.SEMI_PRODUCT;
        };
    }

    /**
     * Proto カテゴリ -> ドメインカテゴリ
     */
    public ItemCategory toDomainCategory(com.example.production.grpc.ItemCategory proto) {
        return switch (proto) {
            case PRODUCT -> ItemCategory.PRODUCT;
            case SEMI_PRODUCT -> ItemCategory.SEMI_PRODUCT;
            case COMPONENT -> ItemCategory.PART;
            case RAW_MATERIAL -> ItemCategory.RAW_MATERIAL;
            case PURCHASED_PART -> ItemCategory.SUPPLY;
            default -> ItemCategory.PRODUCT;
        };
    }

    // ========== Utility ==========

    /**
     * null を空文字列に変換
     */
    public String nullToEmpty(String value) {
        return value == null ? "" : value;
    }

    /**
     * 空文字列を null に変換
     */
    public String emptyToNull(String value) {
        return value == null || value.isEmpty() ? null : value;
    }

    /**
     * null を 0 に変換
     */
    public int nullToZero(Integer value) {
        return value == null ? 0 : value;
    }

    /**
     * 0 を null に変換
     */
    public Integer zeroToNull(int value) {
        return value == 0 ? null : value;
    }
}
