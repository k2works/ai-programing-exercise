package mrs.infrastructure.in.web.form;

import jakarta.validation.constraints.NotNull;
import mrs.common.validation.EndTimeMustBeAfterStartTime;
import mrs.common.validation.ThirtyMinutesUnit;
import mrs.common.validation.TimeRange;

import java.time.LocalTime;

@EndTimeMustBeAfterStartTime
public class ReservationForm implements TimeRange {
    
    @NotNull(message = "Start time is required")
    @ThirtyMinutesUnit
    private LocalTime startTime;
    
    @NotNull(message = "End time is required")
    @ThirtyMinutesUnit
    private LocalTime endTime;

    public ReservationForm() {
    }

    public ReservationForm(LocalTime startTime, LocalTime endTime) {
        this.startTime = startTime;
        this.endTime = endTime;
    }

    @Override
    public LocalTime getStartTime() {
        return startTime;
    }

    public void setStartTime(LocalTime startTime) {
        this.startTime = startTime;
    }

    @Override
    public LocalTime getEndTime() {
        return endTime;
    }

    public void setEndTime(LocalTime endTime) {
        this.endTime = endTime;
    }
}
