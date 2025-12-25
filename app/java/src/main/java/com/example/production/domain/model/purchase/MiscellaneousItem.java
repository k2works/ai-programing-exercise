package com.example.production.domain.model.purchase;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class MiscellaneousItem {
    private Integer id;
    private String purchaseOrderNumber;
    private Integer lineNumber;
    private String itemCode;
    private String itemName;
    private String specification;
    private String drawingNumberMaker;
    private String version;
    private LocalDateTime createdAt;
    private LocalDateTime updatedAt;
}
