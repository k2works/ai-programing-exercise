package mrs.port.in;

import mrs.domain.model.room.MeetingRoom;
import mrs.domain.model.room.ReservableRoom;
import java.time.LocalDate;
import java.util.List;

public interface RoomUseCase {
    List<MeetingRoom> findAllMeetingRooms();
    List<ReservableRoom> findReservableRooms(LocalDate date);
    MeetingRoom findMeetingRoom(Integer roomId);
}