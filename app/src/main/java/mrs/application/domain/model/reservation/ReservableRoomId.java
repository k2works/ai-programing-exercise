package mrs.application.domain.model.reservation;

import mrs.application.domain.model.room.RoomId;
import java.time.LocalDate;

public record ReservableRoomId(LocalDate reservedDate, RoomId roomId) {
    public ReservableRoomId {
        if (reservedDate == null) {
            throw new IllegalArgumentException("Reserved date cannot be null");
        }
        if (roomId == null) {
            throw new IllegalArgumentException("Room ID cannot be null");
        }
    }
}
