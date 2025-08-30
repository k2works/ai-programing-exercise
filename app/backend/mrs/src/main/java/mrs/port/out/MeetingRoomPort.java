package mrs.port.out;

import mrs.domain.model.room.MeetingRoom;
import java.util.List;

public interface MeetingRoomPort {
    List<MeetingRoom> findAll();
    MeetingRoom findById(Integer roomId);
}