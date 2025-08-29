package mrs.infrastructure.out.db;

import java.time.LocalDate;
import java.util.List;
import mrs.application.domain.model.MeetingRoom;
import mrs.application.domain.model.ReservableRoom;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

@Mapper
public interface RoomMapper {
    List<MeetingRoom> findAllRooms();
    List<ReservableRoom> findReservableByDate(@Param("date") LocalDate date);
}
