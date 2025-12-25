package com.example.production.infrastructure.config;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.info.Info;
import io.swagger.v3.oas.models.servers.Server;
import io.swagger.v3.oas.models.tags.Tag;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.util.List;

/**
 * OpenAPI 設定
 */
@Configuration
public class OpenApiConfig {

    @Bean
    public OpenAPI openAPI() {
        return new OpenAPI()
                .info(new Info()
                        .title("生産管理システム API")
                        .description("TDD で育てる生産管理システムの API ドキュメント")
                        .version("1.0.0"))
                .servers(List.of(
                        new Server()
                                .url("http://localhost:8080")
                                .description("開発サーバー")))
                .tags(List.of(
                        new Tag().name("root").description("ルート API"),
                        new Tag().name("items").description("品目マスタ API"),
                        new Tag().name("bom").description("BOM（部品構成表）API"),
                        new Tag().name("suppliers").description("取引先マスタ API"),
                        new Tag().name("orders").description("オーダ API"),
                        new Tag().name("inventory").description("在庫 API"),
                        new Tag().name("work-orders").description("作業指示 API"),
                        new Tag().name("purchase-orders").description("発注 API"),
                        new Tag().name("mrp").description("MRP API")));
    }
}
