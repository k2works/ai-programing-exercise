package mrs.application.domain.model.reservation;

import java.time.LocalDate;

public record ReservedDate(LocalDate value) {
    public ReservedDate {
        if (value == null) {
            throw new IllegalArgumentException("Reserved date cannot be null");
        }
    }
}
