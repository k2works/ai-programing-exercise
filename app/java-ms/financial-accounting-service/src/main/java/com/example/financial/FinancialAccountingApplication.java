package com.example.financial;

import org.mybatis.spring.annotation.MapperScan;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

/**
 * 財務会計サービス
 *
 * <p>
 * 複式簿記に基づく取引記録と財務諸表生成を担当するマイクロサービス。
 * </p>
 *
 * <h3>提供API</h3>
 * <ul>
 *   <li>POST /api/v1/accounts - 勘定科目の作成</li>
 *   <li>GET /api/v1/accounts - 勘定科目の一覧取得</li>
 *   <li>POST /api/v1/journals - 仕訳の作成</li>
 *   <li>GET /api/v1/journals - 仕訳の取得</li>
 *   <li>GET /api/v1/financial-statements/balance-sheet - 貸借対照表の生成</li>
 *   <li>GET /api/v1/financial-statements/income-statement - 損益計算書の生成</li>
 * </ul>
 */
@SpringBootApplication(scanBasePackages = {"com.example.financial", "com.example.accounting"})
@MapperScan("com.example.accounting.infrastructure.adapter.out.persistence.mybatis.mapper")
public class FinancialAccountingApplication {

    public static void main(String[] args) {
        SpringApplication.run(FinancialAccountingApplication.class, args);
    }
}
