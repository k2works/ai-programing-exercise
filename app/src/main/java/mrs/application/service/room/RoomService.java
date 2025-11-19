package mrs.application.service.room;

import mrs.application.domain.model.reservation.*;
import mrs.application.domain.model.room.*;
import mrs.application.port.in.RoomUseCase;
import mrs.application.port.out.MeetingRoomPort;
import mrs.application.port.out.ReservableRoomPort;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(readOnly = true)
public class RoomService implements RoomUseCase {
    private final ReservableRoomPort reservableRoomPort;
    private final MeetingRoomPort meetingRoomPort;

    public RoomService(ReservableRoomPort reservableRoomPort, MeetingRoomPort meetingRoomPort) {
        this.reservableRoomPort = reservableRoomPort;
        this.meetingRoomPort = meetingRoomPort;
    }

    @Override
    public ReservableRoomList findReservableRooms(ReservedDate reservedDate) {
        return reservableRoomPort.findByReservableRoomId_reservedDateOrderByReservableRoomId_roomIdAsc(reservedDate);
    }

    @Override
    public MeetingRoom findMeetingRoom(RoomId roomId) {
        return meetingRoomPort.findById(roomId);
    }
}
