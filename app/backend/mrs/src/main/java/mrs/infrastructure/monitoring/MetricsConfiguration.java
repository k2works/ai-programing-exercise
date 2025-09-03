package mrs.infrastructure.monitoring;

import io.micrometer.core.instrument.MeterRegistry;
import io.micrometer.core.instrument.Timer;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * カスタムメトリクス設定
 * アプリケーション固有のメトリクスを設定
 */
@Configuration
public class MetricsConfiguration {
    
    @Autowired
    private MeterRegistry meterRegistry;
    
    /**
     * 予約関連メトリクス用Timer
     */
    @Bean
    public Timer reservationTimer() {
        return Timer.builder("mrs.reservation.duration")
                .description("Time taken to process reservation operations")
                .tag("operation", "reservation")
                .register(meterRegistry);
    }
    
    /**
     * ユーザー登録関連メトリクス用Timer
     */
    @Bean
    public Timer userRegistrationTimer() {
        return Timer.builder("mrs.user.registration.duration")
                .description("Time taken to process user registration operations")
                .tag("operation", "user_registration")
                .register(meterRegistry);
    }
    
    /**
     * データベースアクセス関連メトリクス用Timer
     */
    @Bean
    public Timer databaseTimer() {
        return Timer.builder("mrs.database.duration")
                .description("Time taken for database operations")
                .tag("operation", "database")
                .register(meterRegistry);
    }
}