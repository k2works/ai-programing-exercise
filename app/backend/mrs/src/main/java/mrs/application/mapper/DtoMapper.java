package mrs.application.mapper;

import edu.umd.cs.findbugs.annotations.SuppressFBWarnings;
import mrs.application.domain.model.room.MeetingRoom;
import mrs.application.domain.model.room.ReservableRoom;
import mrs.application.domain.model.reservation.Reservation;
import mrs.application.dto.MeetingRoomDto;
import mrs.application.dto.ReservableRoomDto;
import mrs.application.dto.ReservationDto;
import mrs.application.dto.ReservationRequest;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Component;

/**
 * ドメインモデルとDTOの相互変換を行うマッパー
 * 各種専用マッパーのファサードクラス
 */
@Component
public class DtoMapper {

    private final MeetingRoomMapper meetingRoomMapper;
    private final ReservableRoomMapper reservableRoomMapper;
    private final ReservationMapper reservationMapper;

    @SuppressFBWarnings(value = "EI_EXPOSE_REP2", 
        justification = "Spring managed beans are effectively immutable")
    public DtoMapper(
        MeetingRoomMapper meetingRoomMapper,
        ReservableRoomMapper reservableRoomMapper,
        @Qualifier("reservationDtoMapper") ReservationMapper reservationMapper
    ) {
        this.meetingRoomMapper = meetingRoomMapper;
        this.reservableRoomMapper = reservableRoomMapper;
        this.reservationMapper = reservationMapper;
    }

    public MeetingRoomDto toMeetingRoomDto(MeetingRoom meetingRoom) {
        return meetingRoomMapper.toDto(meetingRoom);
    }
    
    public MeetingRoomDto toDto(MeetingRoom meetingRoom) {
        return meetingRoomMapper.toDto(meetingRoom);
    }

    public MeetingRoom toDomain(MeetingRoomDto dto) {
        return meetingRoomMapper.toDomain(dto);
    }

    public ReservableRoomDto toReservableRoomDto(ReservableRoom reservableRoom) {
        return reservableRoomMapper.toDto(reservableRoom);
    }

    public ReservableRoom toReservableRoomDomain(ReservableRoomDto dto) {
        return reservableRoomMapper.toDomain(dto);
    }

    public Reservation toReservationDomain(ReservationRequest request) {
        return reservationMapper.fromRequest(request);
    }

    public ReservationDto toReservationDto(Reservation reservation) {
        return reservationMapper.toDto(reservation);
    }
}