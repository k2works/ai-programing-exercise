using System.Diagnostics.Metrics;

namespace MRS.Api.Services;

public interface IMetricsService
{
    void IncrementReservationCreated();
    void IncrementReservationCancelled();
    void IncrementLoginSuccess();
    void IncrementLoginFailure();
    void RecordReservationDuration(double durationMs);
    void RecordActiveUsers(int count);
    
    // 詳細ビジネスメトリクス
    void IncrementReservationConflict();
    void IncrementUnauthorizedAccess();
    void IncrementDatabaseError();
    void RecordRoomUtilization(string roomId, double utilizationPercentage);
    void RecordCancellationReason(string reason);
    void RecordPeakHourUsage(int hour, int reservationCount);
    void RecordUserSessionDuration(double sessionDurationMinutes);
    void IncrementHealthCheckFailure(string checkName);
}

public class MetricsService : IMetricsService
{
    private readonly Counter<int> _reservationCreatedCounter;
    private readonly Counter<int> _reservationCancelledCounter;
    private readonly Counter<int> _loginSuccessCounter;
    private readonly Counter<int> _loginFailureCounter;
    private readonly Histogram<double> _reservationDurationHistogram;
    private readonly Gauge<int> _activeUsersGauge;
    
    // 詳細ビジネスメトリクス
    private readonly Counter<int> _reservationConflictCounter;
    private readonly Counter<int> _unauthorizedAccessCounter;
    private readonly Counter<int> _databaseErrorCounter;
    private readonly Gauge<double> _roomUtilizationGauge;
    private readonly Counter<int> _cancellationReasonCounter;
    private readonly Counter<int> _peakHourUsageCounter;
    private readonly Histogram<double> _userSessionDurationHistogram;
    private readonly Counter<int> _healthCheckFailureCounter;

    public MetricsService(IMeterFactory meterFactory)
    {
        var meter = meterFactory.Create("MRS.Api");
        
        // 基本メトリクス
        _reservationCreatedCounter = meter.CreateCounter<int>(
            "mrs_reservations_created_total",
            description: "Total number of reservations created");
            
        _reservationCancelledCounter = meter.CreateCounter<int>(
            "mrs_reservations_cancelled_total",
            description: "Total number of reservations cancelled");
            
        _loginSuccessCounter = meter.CreateCounter<int>(
            "mrs_logins_success_total",
            description: "Total number of successful logins");
            
        _loginFailureCounter = meter.CreateCounter<int>(
            "mrs_logins_failure_total",
            description: "Total number of failed logins");
            
        _reservationDurationHistogram = meter.CreateHistogram<double>(
            "mrs_reservation_duration_ms",
            description: "Duration of reservation operations in milliseconds");
            
        _activeUsersGauge = meter.CreateGauge<int>(
            "mrs_active_users",
            description: "Number of currently active users");
            
        // 詳細ビジネスメトリクス
        _reservationConflictCounter = meter.CreateCounter<int>(
            "mrs_reservation_conflicts_total",
            description: "Total number of reservation conflicts detected");
            
        _unauthorizedAccessCounter = meter.CreateCounter<int>(
            "mrs_unauthorized_access_total",
            description: "Total number of unauthorized access attempts");
            
        _databaseErrorCounter = meter.CreateCounter<int>(
            "mrs_database_errors_total",
            description: "Total number of database errors encountered");
            
        _roomUtilizationGauge = meter.CreateGauge<double>(
            "mrs_room_utilization_percentage",
            description: "Room utilization percentage by room");
            
        _cancellationReasonCounter = meter.CreateCounter<int>(
            "mrs_cancellation_reasons_total",
            description: "Total number of cancellations by reason");
            
        _peakHourUsageCounter = meter.CreateCounter<int>(
            "mrs_peak_hour_usage_total",
            description: "Total reservations by hour of day");
            
        _userSessionDurationHistogram = meter.CreateHistogram<double>(
            "mrs_user_session_duration_minutes",
            description: "User session duration in minutes");
            
        _healthCheckFailureCounter = meter.CreateCounter<int>(
            "mrs_health_check_failures_total",
            description: "Total number of health check failures");
    }

    // 基本メトリクス実装
    public void IncrementReservationCreated()
        => _reservationCreatedCounter.Add(1);

    public void IncrementReservationCancelled()
        => _reservationCancelledCounter.Add(1);

    public void IncrementLoginSuccess()
        => _loginSuccessCounter.Add(1);

    public void IncrementLoginFailure()
        => _loginFailureCounter.Add(1);

    public void RecordReservationDuration(double durationMs)
        => _reservationDurationHistogram.Record(durationMs);

    public void RecordActiveUsers(int count)
        => _activeUsersGauge.Record(count);
        
    // 詳細ビジネスメトリクス実装
    public void IncrementReservationConflict()
        => _reservationConflictCounter.Add(1);

    public void IncrementUnauthorizedAccess()
        => _unauthorizedAccessCounter.Add(1);

    public void IncrementDatabaseError()
        => _databaseErrorCounter.Add(1);

    public void RecordRoomUtilization(string roomId, double utilizationPercentage)
        => _roomUtilizationGauge.Record(utilizationPercentage, new KeyValuePair<string, object?>("room_id", roomId));

    public void RecordCancellationReason(string reason)
        => _cancellationReasonCounter.Add(1, new KeyValuePair<string, object?>("reason", reason));

    public void RecordPeakHourUsage(int hour, int reservationCount)
        => _peakHourUsageCounter.Add(reservationCount, new KeyValuePair<string, object?>("hour", hour));

    public void RecordUserSessionDuration(double sessionDurationMinutes)
        => _userSessionDurationHistogram.Record(sessionDurationMinutes);

    public void IncrementHealthCheckFailure(string checkName)
        => _healthCheckFailureCounter.Add(1, new KeyValuePair<string, object?>("check_name", checkName));
}