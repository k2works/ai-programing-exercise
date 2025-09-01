package mrs.application.mapper;

import mrs.application.domain.model.reservation.Reservation;
import mrs.application.domain.model.auth.User;
import mrs.application.dto.ReservationDto;
import mrs.application.dto.ReservationRequest;
import org.springframework.stereotype.Component;

/**
 * Reservation関連のマッピング処理
 */
@Component
public class ReservationMapper {

    private final ReservableRoomMapper reservableRoomMapper;

    public ReservationMapper(ReservableRoomMapper reservableRoomMapper) {
        this.reservableRoomMapper = reservableRoomMapper;
    }

    /**
     * ReservationRequestからReservationドメインモデルに変換
     */
    public Reservation fromRequest(ReservationRequest request) {
        if (request == null) {
            return null;
        }
        Reservation reservation = new Reservation();
        reservation.setStartTime(request.getStartTime());
        reservation.setEndTime(request.getEndTime());
        reservation.setReservableRoom(reservableRoomMapper.fromReservationRequest(request));
        return reservation;
    }

    /**
     * ReservationドメインモデルをDTOに変換
     */
    public ReservationDto toDto(Reservation reservation) {
        if (reservation == null) {
            return null;
        }
        ReservationDto dto = new ReservationDto();
        dto.setReservationId(reservation.getReservationId());
        dto.setStartTime(reservation.getStartTime());
        dto.setEndTime(reservation.getEndTime());
        dto.setReservableRoom(reservableRoomMapper.toDto(reservation.getReservableRoom()));
        
        setUserInfo(dto, reservation.getUser());
        
        return dto;
    }

    private void setUserInfo(ReservationDto dto, Object user) {
        if (user != null) {
            User userObj = (User) user;
            dto.setUserId(userObj.getUserId());
            dto.setUserName(userObj.getName());
        }
    }
}