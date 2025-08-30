package mrs.infrastructure.out.persistence;

import mrs.application.domain.model.reservation.Reservation;
import mrs.application.port.out.ReservationPort;
import org.springframework.stereotype.Component;

import java.time.LocalDate;
import java.util.List;

@Component
public class ReservationPersistenceAdapter implements ReservationPort {
    private final ReservationMapper reservationMapper;

    public ReservationPersistenceAdapter(ReservationMapper reservationMapper) {
        this.reservationMapper = reservationMapper;
    }

    @Override
    public List<Reservation> findByRoomIdAndDate(Integer roomId, LocalDate date) {
        return reservationMapper.findByRoomIdAndDate(roomId, date);
    }

    @Override
    public List<Reservation> findByReservableRoomOrderByStartTimeAsc(Integer roomId, LocalDate date) {
        // 開始時刻順はMapperのSQL側で制御
        return reservationMapper.findByRoomIdAndDate(roomId, date);
    }

    @Override
    public Reservation findById(Integer reservationId) {
        return reservationMapper.findById(reservationId);
    }

    @Override
    public Reservation save(Reservation reservation) {
        reservationMapper.insert(reservation);
        return reservation;
    }

    @Override
    public void delete(Integer reservationId) {
        reservationMapper.deleteById(reservationId);
    }
}