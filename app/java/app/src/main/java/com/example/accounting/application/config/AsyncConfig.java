package com.example.accounting.application.config;

import org.springframework.context.annotation.Configuration;
import org.springframework.scheduling.annotation.EnableAsync;

/**
 * 非同期処理設定
 */
@Configuration
@EnableAsync
public class AsyncConfig {
    // @Asyncアノテーションを有効化
}
