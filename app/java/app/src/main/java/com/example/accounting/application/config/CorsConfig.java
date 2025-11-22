package com.example.accounting.application.config;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.cors.CorsConfiguration;
import org.springframework.web.cors.UrlBasedCorsConfigurationSource;
import org.springframework.web.filter.CorsFilter;

import java.util.Arrays;
import java.util.List;

/**
 * CORS設定
 *
 * Script LabやExcel Online等の外部オリジンからのAPIアクセスを許可します。
 */
@Configuration
public class CorsConfig {

    /**
     * CORSフィルタを構成
     *
     * @return CORS設定済みフィルタ
     */
    @Bean
    public CorsFilter corsFilter() {
        UrlBasedCorsConfigurationSource source = new UrlBasedCorsConfigurationSource();
        CorsConfiguration config = new CorsConfiguration();

        // 認証情報の送信を許可
        config.setAllowCredentials(true);

        // Script Lab、Excel Online、開発環境からのアクセスを許可
        config.setAllowedOriginPatterns(Arrays.asList(
            "https://*.cdn.office.net",          // Script Lab
            "https://*.officeapps.live.com",     // Excel Online
            "https://*.office.com",               // Office 365
            "http://localhost:*",                 // ローカル開発
            "https://localhost:*"                 // ローカル開発（HTTPS）
        ));

        // すべてのHTTPメソッドを許可
        config.setAllowedMethods(Arrays.asList(
            "GET", "POST", "PUT", "DELETE", "OPTIONS", "PATCH"
        ));

        // すべてのヘッダーを許可
        config.setAllowedHeaders(List.of("*"));

        // レスポンスヘッダーの公開
        config.setExposedHeaders(Arrays.asList(
            "Authorization",
            "Content-Type",
            "X-Total-Count"
        ));

        // プリフライトリクエストのキャッシュ時間（1時間）
        config.setMaxAge(3600L);

        // すべてのエンドポイントにCORS設定を適用
        source.registerCorsConfiguration("/**", config);

        return new CorsFilter(source);
    }
}
