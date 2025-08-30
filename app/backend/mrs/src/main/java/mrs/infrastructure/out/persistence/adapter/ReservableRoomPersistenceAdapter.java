package mrs.infrastructure.out.persistence.adapter;

import mrs.domain.model.room.ReservableRoom;
import mrs.infrastructure.out.db.RoomMapper;
import mrs.port.out.ReservableRoomPort;
import org.springframework.stereotype.Component;

import java.time.LocalDate;
import java.util.List;

@Component
public class ReservableRoomPersistenceAdapter implements ReservableRoomPort {
    private final RoomMapper roomMapper;

    public ReservableRoomPersistenceAdapter(RoomMapper roomMapper) {
        this.roomMapper = roomMapper;
    }

    @Override
    public List<ReservableRoom> findByReservableDate(LocalDate date) {
        return roomMapper.findReservableByDate(date);
    }

    @Override
    public ReservableRoom findOneForUpdateByReservableRoomId(Integer roomId, LocalDate date) {
        // TODO: FOR UPDATE句を使用した悲観的ロック実装が必要
        return roomMapper.findReservableByRoomIdAndDate(roomId, date);
    }
}