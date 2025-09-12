package com.k2works.mrs.cucumber.steps;

import io.cucumber.java.ja.かつ;
import io.cucumber.java.ja.ならば;
import io.cucumber.java.ja.もし;
import io.cucumber.java.ja.前提;
import io.cucumber.spring.CucumberContextConfiguration;
import org.springframework.boot.actuate.health.HealthEndpoint;
import org.springframework.boot.actuate.health.Status;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.TestPropertySource;

import javax.sql.DataSource;
import java.sql.Connection;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * 会議室予約システム基本動作確認のステップ定義
 * 
 * システムの動作確認レベルの簡単なテストを実装
 */
@CucumberContextConfiguration
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
@TestPropertySource(properties = {
    "spring.datasource.url=jdbc:h2:mem:testdb",
    "spring.jpa.hibernate.ddl-auto=create-drop"
})
public class ReservationSteps {
    
    @Autowired
    private HealthEndpoint healthEndpoint;
    
    @Autowired
    private DataSource dataSource;
    
    private Status healthStatus;
    private boolean databaseConnected;
    
    @前提("アプリケーションが起動している")
    public void アプリケーションが起動している() {
        // Spring Boot テストコンテキストが起動していれば OK
        assertThat(healthEndpoint).isNotNull();
        assertThat(dataSource).isNotNull();
    }
    
    @もし("ヘルスチェックを実行する")
    public void ヘルスチェックを実行する() {
        var health = healthEndpoint.health();
        this.healthStatus = health.getStatus();
    }
    
    @ならば("システムが正常に動作している")
    public void システムが正常に動作している() {
        assertThat(healthStatus).isEqualTo(Status.UP);
    }
    
    @もし("データベースに接続する")
    public void データベースに接続する() throws Exception {
        try (Connection connection = dataSource.getConnection()) {
            this.databaseConnected = connection != null && !connection.isClosed();
        }
    }
    
    @ならば("データベースが利用可能である")
    public void データベースが利用可能である() {
        assertThat(databaseConnected).isTrue();
    }
}