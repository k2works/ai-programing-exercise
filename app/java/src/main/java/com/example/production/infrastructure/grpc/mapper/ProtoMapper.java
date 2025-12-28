package com.example.production.infrastructure.grpc.mapper;

import com.example.production.grpc.Date;
import com.example.production.grpc.Decimal;
import org.springframework.stereotype.Component;

import java.math.BigDecimal;
import java.time.LocalDate;

/**
 * Proto メッセージとドメインオブジェクト間の変換
 */
@Component
public class ProtoMapper {

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
}
