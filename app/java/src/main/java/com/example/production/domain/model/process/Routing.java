package com.example.production.domain.model.process;

import com.example.production.domain.model.item.Item;
import lombok.*;

import java.time.LocalDateTime;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class Routing {
    private Integer id;
    private String itemCode;
    private Integer sequence;
    private String processCode;
    private LocalDateTime createdAt;
    private LocalDateTime updatedAt;

    // リレーション
    private Item item;
    private Process process;
}
