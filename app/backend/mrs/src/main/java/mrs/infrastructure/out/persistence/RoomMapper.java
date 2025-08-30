package mrs.infrastructure.out.persistence;

import java.time.LocalDate;
import java.util.List;
import mrs.application.domain.model.room.MeetingRoom;
import mrs.application.domain.model.room.ReservableRoom;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

@Mapper
public interface RoomMapper {
    List<MeetingRoom> findAllRooms();
    MeetingRoom findRoomById(@Param("roomId") Integer roomId);
    List<ReservableRoom> findReservableByDate(@Param("date") LocalDate date);
    ReservableRoom findReservableByRoomIdAndDate(@Param("roomId") Integer roomId, @Param("date") LocalDate date);
}
