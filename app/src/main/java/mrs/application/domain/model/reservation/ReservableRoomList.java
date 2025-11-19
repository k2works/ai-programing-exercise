package mrs.application.domain.model.reservation;

import java.util.List;

public record ReservableRoomList(List<ReservableRoom> rooms) {
    public int size() {
        return rooms.size();
    }

    public ReservableRoom get(int index) {
        return rooms.get(index);
    }
}
