package mrs.application.port.out;

import mrs.application.domain.model.reservation.*;

public interface ReservableRoomPort {
    ReservableRoom findOneForUpdateByReservableRoomId(ReservableRoomId reservableRoomId);
    ReservableRoomList findByReservableRoomId_reservedDateOrderByReservableRoomId_roomIdAsc(ReservedDate reservedDate);
}
