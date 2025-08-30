package mrs.application.port.out;

import mrs.application.domain.model.room.MeetingRoom;
import java.util.List;

public interface MeetingRoomPort {
    List<MeetingRoom> findAll();
    MeetingRoom findById(Integer roomId);
}