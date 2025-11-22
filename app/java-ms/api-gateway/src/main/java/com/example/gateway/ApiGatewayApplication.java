package com.example.gateway;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

/**
 * API Gateway
 *
 * <p>
 * マイクロサービスへのリクエストをルーティングするゲートウェイ。
 * Spring Cloud Gateway を使用してルーティング、認証、レート制限などを実現します。
 * </p>
 *
 * <h3>ルーティング</h3>
 * <ul>
 *   <li>/api/v1/accounts/** → 財務会計サービス (Port 8081)</li>
 *   <li>/api/v1/journals/** → 財務会計サービス (Port 8081)</li>
 *   <li>/api/v1/financial-statements/** → 財務会計サービス (Port 8081)</li>
 *   <li>/api/v1/financial-analysis/** → 管理会計サービス (Port 8082)</li>
 *   <li>/api/v1/financial-ratios/** → 管理会計サービス (Port 8082)</li>
 * </ul>
 */
@SpringBootApplication
public class ApiGatewayApplication {

    public static void main(String[] args) {
        SpringApplication.run(ApiGatewayApplication.class, args);
    }
}
