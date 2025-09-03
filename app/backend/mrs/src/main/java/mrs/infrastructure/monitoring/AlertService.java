package mrs.infrastructure.monitoring;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.scheduling.annotation.Async;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;

import javax.sql.DataSource;
import java.sql.Connection;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * 基本アラート機能サービス
 * システムの異常状態を監視してアラートを発生させる
 */
@Service
public class AlertService {
    
    private static final Logger logger = LoggerFactory.getLogger(AlertService.class);
    private static final Logger alertLogger = LoggerFactory.getLogger("ALERT");
    
    private final DataSource dataSource;
    private final Map<String, AlertState> alertStates = new ConcurrentHashMap<>();
    
    public AlertService(DataSource dataSource) {
        this.dataSource = dataSource;
    }
    
    /**
     * システム監視とアラート（5分間隔）
     */
    @Scheduled(fixedRate = 300000) // 5分間隔
    public void monitorSystemHealth() {
        try {
            checkDatabaseConnection();
            checkMemoryUsage();
            checkCpuUsage();
        } catch (Exception e) {
            logger.error("Health monitoring failed", e);
            sendAlert("SYSTEM_ERROR", "Critical", 
                "Health monitoring system failed: " + e.getMessage());
        }
    }
    
    private void checkDatabaseConnection() {
        String alertKey = "DATABASE_CONNECTION";
        
        try (Connection connection = dataSource.getConnection()) {
            if (connection.isValid(5)) {
                clearAlert(alertKey);
            } else {
                if (shouldSendAlert(alertKey)) {
                    sendAlert(alertKey, "Critical", "Database connection is not valid");
                    updateAlertState(alertKey, AlertSeverity.CRITICAL);
                }
            }
        } catch (Exception e) {
            if (shouldSendAlert(alertKey)) {
                sendAlert(alertKey, "Critical", 
                    "Database connection failed: " + e.getMessage());
                updateAlertState(alertKey, AlertSeverity.CRITICAL);
            }
        }
    }
    
    private void checkMemoryUsage() {
        String alertKey = "MEMORY_USAGE";
        
        Runtime runtime = Runtime.getRuntime();
        long maxMemory = runtime.maxMemory();
        long totalMemory = runtime.totalMemory();
        long freeMemory = runtime.freeMemory();
        long usedMemory = totalMemory - freeMemory;
        
        double memoryUsagePercent = (double) usedMemory / maxMemory * 100;
        
        if (memoryUsagePercent > 90) {
            if (shouldSendAlert(alertKey)) {
                sendAlert(alertKey, "Critical", 
                    String.format("Memory usage is critical: %.2f%%", memoryUsagePercent));
                updateAlertState(alertKey, AlertSeverity.CRITICAL);
            }
        } else if (memoryUsagePercent > 80) {
            if (shouldSendAlert(alertKey)) {
                sendAlert(alertKey, "Warning", 
                    String.format("Memory usage is high: %.2f%%", memoryUsagePercent));
                updateAlertState(alertKey, AlertSeverity.WARNING);
            }
        } else {
            clearAlert(alertKey);
        }
    }
    
    private void checkCpuUsage() {
        String alertKey = "CPU_USAGE";
        
        try {
            double cpuUsage = ((com.sun.management.OperatingSystemMXBean) 
                java.lang.management.ManagementFactory.getOperatingSystemMXBean())
                .getProcessCpuLoad() * 100;
            
            if (cpuUsage >= 0) {
                if (cpuUsage > 90) {
                    if (shouldSendAlert(alertKey)) {
                        sendAlert(alertKey, "Critical", 
                            String.format("CPU usage is critical: %.2f%%", cpuUsage));
                        updateAlertState(alertKey, AlertSeverity.CRITICAL);
                    }
                } else if (cpuUsage > 80) {
                    if (shouldSendAlert(alertKey)) {
                        sendAlert(alertKey, "Warning", 
                            String.format("CPU usage is high: %.2f%%", cpuUsage));
                        updateAlertState(alertKey, AlertSeverity.WARNING);
                    }
                } else {
                    clearAlert(alertKey);
                }
            }
        } catch (Exception e) {
            logger.warn("Failed to check CPU usage", e);
        }
    }
    
    private boolean shouldSendAlert(String alertKey) {
        AlertState state = alertStates.get(alertKey);
        if (state == null) {
            return true;
        }
        
        // 同じアラートは1時間以内は再送しない
        return state.getLastSentTime().isBefore(LocalDateTime.now().minusHours(1));
    }
    
    private void updateAlertState(String alertKey, AlertSeverity severity) {
        alertStates.put(alertKey, new AlertState(severity, LocalDateTime.now()));
    }
    
    private void clearAlert(String alertKey) {
        AlertState state = alertStates.remove(alertKey);
        if (state != null && state.getSeverity() != AlertSeverity.NONE) {
            sendAlert(alertKey, "Info", 
                String.format("Alert %s has been resolved", alertKey));
        }
    }
    
    @Async
    private void sendAlert(String alertKey, String severity, String message) {
        String timestamp = LocalDateTime.now().format(DateTimeFormatter.ISO_LOCAL_DATE_TIME);
        String alertMessage = String.format(
            "[%s] ALERT [%s] %s: %s", 
            timestamp, alertKey, severity, message
        );
        
        // ログへの出力
        alertLogger.error(alertMessage);
        
        // 将来的にはメール、Slack、SMS等への通知も実装可能
        logger.info("Alert sent: {}", alertMessage);
    }
    
    /**
     * アラート状態クラス
     */
    private static class AlertState {
        private final AlertSeverity severity;
        private final LocalDateTime lastSentTime;
        
        public AlertState(AlertSeverity severity, LocalDateTime lastSentTime) {
            this.severity = severity;
            this.lastSentTime = lastSentTime;
        }
        
        public AlertSeverity getSeverity() {
            return severity;
        }
        
        public LocalDateTime getLastSentTime() {
            return lastSentTime;
        }
    }
    
    /**
     * アラート重要度
     */
    public enum AlertSeverity {
        NONE, WARNING, CRITICAL
    }
}