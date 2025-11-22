package com.example.management;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

/**
 * 管理会計サービス
 *
 * <p>
 * 財務分析と経営指標の計算を担当するマイクロサービス。
 * 財務会計サービスから財務データを取得し、分析結果を提供します。
 * </p>
 *
 * <h3>提供API</h3>
 * <ul>
 *   <li>GET /api/v1/financial-analysis/{fiscalYear} - 財務分析の実行</li>
 *   <li>GET /api/v1/financial-analysis/compare - 複数期間の比較分析</li>
 *   <li>GET /api/v1/financial-ratios/{fiscalYear} - 財務比率の計算</li>
 * </ul>
 */
@SpringBootApplication
public class ManagementAccountingApplication {

    public static void main(String[] args) {
        SpringApplication.run(ManagementAccountingApplication.class, args);
    }
}
