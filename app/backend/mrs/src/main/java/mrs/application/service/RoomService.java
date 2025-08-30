package mrs.application.service;

import mrs.port.in.RoomUseCase;
import mrs.domain.model.room.MeetingRoom;
import mrs.domain.model.room.ReservableRoom;
import mrs.port.out.MeetingRoomPort;
import mrs.port.out.ReservableRoomPort;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDate;
import java.util.List;

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
    public List<MeetingRoom> findAllMeetingRooms() {
        return meetingRoomPort.findAll();
    }

    @Override
    public List<ReservableRoom> findReservableRooms(LocalDate date) {
        return reservableRoomPort.findByReservableDate(date);
    }

    @Override
    public MeetingRoom findMeetingRoom(Integer roomId) {
        return meetingRoomPort.findById(roomId);
    }
}