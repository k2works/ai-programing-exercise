package mrs.application.port.in;

import mrs.application.domain.model.reservation.*;
import mrs.application.domain.model.room.*;

public interface RoomUseCase {
    ReservableRoomList findReservableRooms(ReservedDate reservedDate);
    MeetingRoom findMeetingRoom(RoomId roomId);
}
