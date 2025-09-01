package mrs.application.mapper;

import mrs.application.domain.model.room.MeetingRoom;
import mrs.application.dto.MeetingRoomDto;
import org.springframework.stereotype.Component;

/**
 * MeetingRoom関連のマッピング処理
 */
@Component
public class MeetingRoomMapper {

    /**
     * MeetingRoomドメインモデルをDTOに変換
     */
    public MeetingRoomDto toDto(MeetingRoom meetingRoom) {
        if (meetingRoom == null) {
            return null;
        }
        return new MeetingRoomDto(
            meetingRoom.getRoomId(),
            meetingRoom.getRoomName()
        );
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
}