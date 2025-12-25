package com.example.production.domain.model.inventory;

import lombok.*;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.List;

/**
 * 棚卸データエンティティ
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class Stocktaking {
    private Long id;
    private String stocktakingNumber;
    private String locationCode;
    private LocalDate stocktakingDate;
    private StocktakingStatus status;
    private LocalDateTime createdAt;
    private LocalDateTime updatedAt;

    private List<StocktakingDetail> details;
}
