package mrs.application.domain.model.reservation;

import java.time.LocalTime;

public record ReservationTimeSlot(LocalTime startTime, LocalTime endTime) {
    public ReservationTimeSlot {
        if (startTime == null) {
            throw new IllegalArgumentException("Start time cannot be null");
        }
        if (endTime == null) {
            throw new IllegalArgumentException("End time cannot be null");
        }
        if (endTime.isBefore(startTime) || endTime.equals(startTime)) {
            throw new IllegalArgumentException("End time must be after start time");
        }
    }

    public boolean overlap(ReservationTimeSlot other) {
        return this.startTime.isBefore(other.endTime) && other.startTime.isBefore(this.endTime);
    }
}
