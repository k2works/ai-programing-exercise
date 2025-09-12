package com.k2works.mrs.cucumber;

import io.cucumber.spring.CucumberContextConfiguration;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.ActiveProfiles;

/**
 * Cucumber と Spring Boot の統合設定
 * 
 * Spring アプリケーションコンテキストを Cucumber テストで使用するための設定
 * テスト用プロファイルを適用し、H2 データベースを使用
 */
@CucumberContextConfiguration
@SpringBootTest
@ActiveProfiles("test")
public class CucumberSpringConfiguration {
    // この設定クラスは Spring Boot テストコンテキストを
    // Cucumber テストで利用可能にする
    // 実際のテストロジックは StepDefinitions で実装される
}