package mrs.infrastructure.out.persistence;

import mrs.application.domain.model.auth.UserId;
import mrs.application.domain.model.reservation.*;
import mrs.application.domain.model.room.RoomId;
import mrs.application.port.out.ReservationPort;
import mrs.infrastructure.out.persistence.entity.ReservationEntity;
import org.springframework.stereotype.Component;

import java.util.List;

@Component
public class ReservationPersistenceAdapter implements ReservationPort {
    private final ReservationJpaRepository repository;

    public ReservationPersistenceAdapter(ReservationJpaRepository repository) {
        this.repository = repository;
    }

    @Override
    public List<Reservation> findByReservableRoom_ReservableRoomIdOrderByStartTimeAsc(ReservableRoomId reservableRoomId) {
        return repository.findByReservedDateAndRoomIdOrderByStartTimeAsc(
            reservableRoomId.reservedDate(),
            reservableRoomId.roomId().value()
        ).stream()
            .map(this::toDomain)
            .toList();
    }

    @Override
    public void save(Reservation reservation) {
        repository.save(toEntity(reservation));
    }

    @Override
    public void delete(Reservation reservation) {
        repository.deleteById(reservation.reservationId().value());
    }

    @Override
    public Reservation findById(Integer id) {
        return repository.findById(id)
            .map(this::toDomain)
            .orElse(null);
    }

    private Reservation toDomain(ReservationEntity entity) {
        return new Reservation(
            new ReservationId(entity.getReservationId()),
            new ReservationTimeSlot(entity.getStartTime(), entity.getEndTime()),
            new ReservableRoomId(entity.getReservedDate(), new RoomId(entity.getRoomId())),
            new UserId(entity.getUserId())
        );
    }

    private ReservationEntity toEntity(Reservation reservation) {
        return new ReservationEntity(
            reservation.reservationId().value(),
            reservation.timeSlot().startTime(),
            reservation.timeSlot().endTime(),
            reservation.reservableRoomId().reservedDate(),
            reservation.reservableRoomId().roomId().value(),
            reservation.userId().value()
        );
    }
}
