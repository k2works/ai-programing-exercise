package mrs.infrastructure.out.persistence;

import mrs.application.domain.model.room.MeetingRoom;
import mrs.application.port.out.MeetingRoomPort;
import org.springframework.stereotype.Component;

import java.util.List;

@Component
public class MeetingRoomPersistenceAdapter implements MeetingRoomPort {
    private final RoomMapper roomMapper;

    public MeetingRoomPersistenceAdapter(RoomMapper roomMapper) {
        this.roomMapper = roomMapper;
    }

    @Override
    public List<MeetingRoom> findAll() {
        return roomMapper.findAllRooms();
    }

    @Override
    public MeetingRoom findById(Integer roomId) {
        return roomMapper.findRoomById(roomId);
    }
}