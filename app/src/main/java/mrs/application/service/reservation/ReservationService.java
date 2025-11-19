package mrs.application.service.reservation;

import mrs.application.domain.model.auth.User;
import mrs.application.domain.model.reservation.*;
import mrs.application.port.in.ReservationUseCase;
import mrs.application.port.out.ReservableRoomPort;
import mrs.application.port.out.ReservationPort;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

@Service
@Transactional
public class ReservationService implements ReservationUseCase {
    private final ReservationPort reservationPort;
    private final ReservableRoomPort reservableRoomPort;

    public ReservationService(ReservationPort reservationPort, ReservableRoomPort reservableRoomPort) {
        this.reservationPort = reservationPort;
        this.reservableRoomPort = reservableRoomPort;
    }

    @Override
    public ReservationList findReservations(ReservableRoomId reservableRoomId) {
        List<Reservation> reservations = reservationPort
            .findByReservableRoom_ReservableRoomIdOrderByStartTimeAsc(reservableRoomId);
        return new ReservationList(reservations);
    }

    @Override
    public Reservation reserve(Reservation reservation) {
        ReservableRoom reservableRoom = reservableRoomPort
            .findOneForUpdateByReservableRoomId(reservation.reservableRoomId());
        
        if (reservableRoom == null) {
            throw new UnavailableReservationException("Reservable room not found");
        }

        List<Reservation> existingReservations = reservationPort
            .findByReservableRoom_ReservableRoomIdOrderByStartTimeAsc(reservation.reservableRoomId());
        
        for (Reservation existing : existingReservations) {
            if (reservation.overlap(existing)) {
                throw new AlreadyReservedException("Already reserved");
            }
        }

        reservationPort.save(reservation);
        return reservation;
    }

    @Override
    public void cancel(Reservation reservation) {
        reservationPort.delete(reservation);
    }

    @Override
    public Reservation findOne(ReservationId reservationId) {
        return reservationPort.findById(reservationId.value());
    }

    public boolean canCancel(Reservation reservation, User user) {
        return user.isAdmin() || reservation.userId().equals(user.userId());
    }
}
