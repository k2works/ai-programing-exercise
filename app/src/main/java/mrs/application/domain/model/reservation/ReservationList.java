package mrs.application.domain.model.reservation;

import java.util.List;

public record ReservationList(List<Reservation> reservations) {
    public int size() {
        return reservations.size();
    }

    public Reservation get(int index) {
        return reservations.get(index);
    }
}
