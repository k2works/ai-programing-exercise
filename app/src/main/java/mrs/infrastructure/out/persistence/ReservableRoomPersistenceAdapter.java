package mrs.infrastructure.out.persistence;

import mrs.application.domain.model.reservation.*;
import mrs.application.domain.model.room.*;
import mrs.application.port.out.ReservableRoomPort;
import mrs.infrastructure.out.persistence.entity.ReservableRoomEntity;
import org.springframework.stereotype.Component;

import java.util.List;

@Component
public class ReservableRoomPersistenceAdapter implements ReservableRoomPort {
    private final ReservableRoomJpaRepository repository;

    public ReservableRoomPersistenceAdapter(ReservableRoomJpaRepository repository) {
        this.repository = repository;
    }

    @Override
    public ReservableRoom findOneForUpdateByReservableRoomId(ReservableRoomId reservableRoomId) {
        ReservableRoomEntity entity = repository.findOneForUpdate(
            reservableRoomId.reservedDate(),
            reservableRoomId.roomId().value()
        );
        return entity != null ? toDomain(entity) : null;
    }

    @Override
    public ReservableRoomList findByReservableRoomId_reservedDateOrderByReservableRoomId_roomIdAsc(ReservedDate reservedDate) {
        List<ReservableRoom> rooms = repository.findByReservedDateOrderByRoomIdAsc(reservedDate.value())
            .stream()
            .map(this::toDomain)
            .toList();
        return new ReservableRoomList(rooms);
    }

    private ReservableRoom toDomain(ReservableRoomEntity entity) {
        ReservableRoomId id = new ReservableRoomId(
            entity.getReservedDate(),
            new RoomId(entity.getRoomId())
        );
        MeetingRoom meetingRoom = new MeetingRoom(
            new RoomId(entity.getMeetingRoom().getRoomId()),
            new RoomName(entity.getMeetingRoom().getRoomName())
        );
        return new ReservableRoom(id, meetingRoom);
    }
}
