package mrs.application.port.in;

import mrs.application.domain.model.room.MeetingRoom;
import mrs.application.domain.model.room.ReservableRoom;
import java.time.LocalDate;
import java.util.List;

public interface RoomUseCase {
    List<MeetingRoom> findAllMeetingRooms();
    List<ReservableRoom> findReservableRooms(LocalDate date);
    MeetingRoom findMeetingRoom(Integer roomId);
}