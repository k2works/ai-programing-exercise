package mrs.application.mapper;

import mrs.application.domain.model.room.ReservableRoom;
import mrs.application.dto.ReservableRoomDto;
import mrs.application.dto.ReservationRequest;
import org.springframework.stereotype.Component;

/**
 * ReservableRoom関連のマッピング処理
 */
@Component
public class ReservableRoomMapper {

    /**
     * ReservableRoomドメインモデルをDTOに変換
     */
    public ReservableRoomDto toDto(ReservableRoom reservableRoom) {
        if (reservableRoom == null) {
            return null;
        }
        return new ReservableRoomDto(
            reservableRoom.getRoomId(),
            reservableRoom.getReservableDate()
        );
    }

    /**
     * DTOからReservableRoomドメインモデルに変換
     */
    public ReservableRoom toDomain(ReservableRoomDto dto) {
        if (dto == null) {
            return null;
        }
        ReservableRoom reservableRoom = new ReservableRoom();
        reservableRoom.setRoomId(dto.getRoomId());
        reservableRoom.setReservableDate(dto.getReservableDate());
        return reservableRoom;
    }

    /**
     * ReservationRequestからReservableRoomドメインモデルに変換
     */
    public ReservableRoom fromReservationRequest(ReservationRequest request) {
        if (request == null) {
            return null;
        }
        ReservableRoom reservableRoom = new ReservableRoom();
        reservableRoom.setRoomId(request.getRoomId());
        reservableRoom.setReservableDate(request.getReservableDate());
        return reservableRoom;
    }
}