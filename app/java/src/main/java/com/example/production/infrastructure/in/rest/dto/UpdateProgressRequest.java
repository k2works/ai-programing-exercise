package com.example.production.infrastructure.in.rest.dto;

import com.example.production.domain.model.process.WorkOrderStatus;
import jakarta.validation.constraints.NotNull;
import lombok.Data;

/**
 * 進捗更新リクエスト
 */
@Data
public class UpdateProgressRequest {

    @NotNull(message = "ステータスは必須です")
    private WorkOrderStatus status;
}
