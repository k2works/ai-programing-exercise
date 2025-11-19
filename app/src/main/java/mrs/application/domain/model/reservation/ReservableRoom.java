package mrs.application.domain.model.reservation;

import mrs.application.domain.model.room.MeetingRoom;

public record ReservableRoom(ReservableRoomId reservableRoomId, MeetingRoom meetingRoom) {
    public ReservableRoom {
        if (reservableRoomId == null) {
            throw new IllegalArgumentException("ReservableRoomId cannot be null");
        }
        if (meetingRoom == null) {
            throw new IllegalArgumentException("MeetingRoom cannot be null");
        }
    }
}
