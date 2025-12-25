package com.example.production.application.port.in.command;

import lombok.Builder;
import lombok.Data;

/**
 * 欠点マスタ登録コマンド
 */
@Data
@Builder
public class DefectMasterCreateCommand {
    private String defectCode;
    private String defectDescription;
}
