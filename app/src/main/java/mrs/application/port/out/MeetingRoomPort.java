package mrs.application.port.out;

import mrs.application.domain.model.room.*;
import java.util.List;

public interface MeetingRoomPort {
    MeetingRoom findById(RoomId roomId);
    List<MeetingRoom> findAll();
}
