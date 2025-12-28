package com.example.production.infrastructure.web.form;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import lombok.Data;

import java.time.LocalDate;

/**
 * 棚卸表発行フォーム
 */
@Data
public class StocktakingForm {

    @NotBlank(message = "場所コードは必須です")
    private String locationCode;

    @NotNull(message = "棚卸日は必須です")
    private LocalDate stocktakingDate;
}
