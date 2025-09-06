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
}

public class MetricsService : IMetricsService
{
    private readonly Counter<int> _reservationCreatedCounter;
    private readonly Counter<int> _reservationCancelledCounter;
    private readonly Counter<int> _loginSuccessCounter;
    private readonly Counter<int> _loginFailureCounter;
    private readonly Histogram<double> _reservationDurationHistogram;
    private readonly Gauge<int> _activeUsersGauge;

    public MetricsService(IMeterFactory meterFactory)
    {
        var meter = meterFactory.Create("MRS.Api");
        
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
    }

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
}