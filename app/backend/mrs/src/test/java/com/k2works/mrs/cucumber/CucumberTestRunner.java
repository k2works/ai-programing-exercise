package com.k2works.mrs.cucumber;

import org.junit.platform.suite.api.ConfigurationParameter;
import org.junit.platform.suite.api.IncludeEngines;
import org.junit.platform.suite.api.SelectClasspathResource;
import org.junit.platform.suite.api.Suite;

import static io.cucumber.junit.platform.engine.Constants.PLUGIN_PROPERTY_NAME;
import static io.cucumber.junit.platform.engine.Constants.GLUE_PROPERTY_NAME;

/**
 * Cucumber BDD テストランナー
 * 
 * 会議室予約システムの受け入れテストを実行する
 * JUnit 5 Platform との統合によりGradleテストタスクから実行可能
 */
@Suite
@IncludeEngines("cucumber")
@SelectClasspathResource("features")
@ConfigurationParameter(key = PLUGIN_PROPERTY_NAME, value = 
    "pretty," +
    "html:build/reports/cucumber-reports/html," +
    "json:build/reports/cucumber-reports/json/cucumber.json," +
    "junit:build/reports/cucumber-reports/junit/cucumber.xml")
@ConfigurationParameter(key = GLUE_PROPERTY_NAME, value = "com.k2works.mrs.cucumber.steps")
public class CucumberTestRunner {
    // このクラスは Cucumber テストの設定のみを行う
    // 実際のテストは .feature ファイルとステップ定義で実装される
}