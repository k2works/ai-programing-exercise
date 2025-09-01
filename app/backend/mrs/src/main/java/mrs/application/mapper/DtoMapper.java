package mrs.application.mapper;

import mrs.application.domain.model.room.MeetingRoom;
import mrs.application.domain.model.room.ReservableRoom;
import mrs.application.domain.model.reservation.Reservation;
import mrs.application.dto.MeetingRoomDto;
import mrs.application.dto.ReservableRoomDto;
import mrs.application.dto.ReservationDto;
import mrs.application.dto.ReservationRequest;
import org.springframework.stereotype.Component;

/**
 * ドメインモデルとDTOの相互変換を行うマッパー
 * Application層内でドメイン境界を管理するためのマッパー
 */
@Component
public class DtoMapper {

    /**
     * MeetingRoomドメインモデルをDTOに変換
     */
    public MeetingRoomDto toMeetingRoomDto(MeetingRoom meetingRoom) {
        if (meetingRoom == null) {
            return null;
        }
        return new MeetingRoomDto(
            meetingRoom.getRoomId(),
            meetingRoom.getRoomName()
        );
    }
    
    /**
     * MeetingRoomドメインモデルをDTOに変換 (互換性のため)
     */
    public MeetingRoomDto toDto(MeetingRoom meetingRoom) {
        return toMeetingRoomDto(meetingRoom);
    }

    /**
     * DTOからMeetingRoomドメインモデルに変換
     */
    public MeetingRoom toDomain(MeetingRoomDto dto) {
        if (dto == null) {
            return null;
        }
        MeetingRoom meetingRoom = new MeetingRoom();
        meetingRoom.setRoomId(dto.getRoomId());
        meetingRoom.setRoomName(dto.getRoomName());
        return meetingRoom;
    }

    /**
     * ReservableRoomドメインモデルをDTOに変換
     */
    public ReservableRoomDto toReservableRoomDto(ReservableRoom reservableRoom) {
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
    public ReservableRoom toReservableRoomDomain(ReservableRoomDto dto) {
        if (dto == null) {
            return null;
        }
        ReservableRoom reservableRoom = new ReservableRoom();
        reservableRoom.setRoomId(dto.getRoomId());
        reservableRoom.setReservableDate(dto.getReservableDate());
        return reservableRoom;
    }

    /**
     * ReservationRequestからReservationドメインモデルに変換
     */
    public Reservation toReservationDomain(ReservationRequest request) {
        if (request == null) {
            return null;
        }
        Reservation reservation = new Reservation();
        reservation.setStartTime(request.getStartTime());
        reservation.setEndTime(request.getEndTime());
        
        // ReservableRoomを設定
        ReservableRoom reservableRoom = new ReservableRoom();
        reservableRoom.setRoomId(request.getRoomId());
        reservableRoom.setReservableDate(request.getReservableDate());
        reservation.setReservableRoom(reservableRoom);
        
        return reservation;
    }

    /**
     * ReservationドメインモデルをDTOに変換
     */
    public ReservationDto toReservationDto(Reservation reservation) {
        if (reservation == null) {
            return null;
        }
        ReservationDto dto = new ReservationDto();
        dto.setReservationId(reservation.getReservationId());
        dto.setStartTime(reservation.getStartTime());
        dto.setEndTime(reservation.getEndTime());
        dto.setReservableRoom(toReservableRoomDto(reservation.getReservableRoom()));
        
        if (reservation.getUser() != null) {
            dto.setUserId(reservation.getUser().getUserId());
            dto.setUserName(reservation.getUser().getName());
        }
        
        return dto;
    }
}