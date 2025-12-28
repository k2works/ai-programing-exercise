package com.example.production.infrastructure.in.seed;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.ApplicationArguments;
import org.springframework.boot.ApplicationRunner;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.stereotype.Component;

/**
 * アプリケーション起動時にシードデータを投入する
 *
 * application.yml で seed.enabled=true を設定すると有効になる
 */
@Component
@ConditionalOnProperty(name = "seed.enabled", havingValue = "true")
@RequiredArgsConstructor
@Slf4j
public class SeedDataRunner implements ApplicationRunner {

    private final SeedDataService seedDataService;

    @Override
    public void run(ApplicationArguments args) {
        log.info("シードデータ投入を開始します...");
        seedDataService.seedAll();
        log.info("シードデータ投入が完了しました");
    }
}
