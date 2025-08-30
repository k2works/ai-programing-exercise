package mrs.application.mapper;

import mrs.application.domain.model.room.MeetingRoom;
import mrs.application.domain.model.room.ReservableRoom;
import mrs.application.dto.MeetingRoomDto;
import mrs.application.dto.ReservableRoomDto;
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
}