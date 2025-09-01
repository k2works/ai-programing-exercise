package mrs.application.service;

import edu.umd.cs.findbugs.annotations.SuppressFBWarnings;
import mrs.common.exception.AlreadyReservedException;
import mrs.common.exception.ReservationNotFoundException;
import mrs.common.exception.UnavailableReservationException;
import mrs.application.port.in.ReservationUseCase;
import mrs.application.domain.model.auth.User;
import mrs.application.domain.model.reservation.Reservation;
import mrs.application.domain.model.room.ReservableRoom;
import mrs.application.port.out.ReservableRoomPort;
import mrs.application.port.out.ReservationPort;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDate;
import java.util.List;

@Service
@Transactional
public class ReservationService implements ReservationUseCase {
    private final ReservationPort reservationPort;
    private final ReservableRoomPort reservableRoomPort;

    @SuppressFBWarnings(value = "EI_EXPOSE_REP2", justification = "Dependencies are managed by Spring DI container and are safe")
    public ReservationService(ReservationPort reservationPort, ReservableRoomPort reservableRoomPort) {
        this.reservationPort = reservationPort;
        this.reservableRoomPort = reservableRoomPort;
    }

    @Override
    @Transactional(readOnly = true)
    public List<Reservation> findReservations(Integer roomId, LocalDate date) {
        return reservationPort.findByRoomIdAndDate(roomId, date);
    }

    @Override
    public Reservation reserve(Reservation reservation, User user) {
        // 悲観的ロックで予約可能会議室を取得
        ReservableRoom reservableRoom = reservableRoomPort.findOneForUpdateByReservableRoomId(
            reservation.getReservableRoom().getRoomId(),
            reservation.getReservableRoom().getReservableDate()
        );
        
        if (reservableRoom == null) {
            throw new UnavailableReservationException("指定の日付・部屋の組合わせは予約できません。");
        }
        
        // 既存予約を取得して重複チェック
        List<Reservation> existingReservations = reservationPort.findByReservableRoomOrderByStartTimeAsc(
            reservableRoom.getRoomId(),
            reservableRoom.getReservableDate()
        );
        
        for (Reservation existing : existingReservations) {
            if (reservation.overlap(existing)) {
                throw new AlreadyReservedException("入力の時間帯はすでに予約済みです。");
            }
        }
        
        reservation.setUser(user);
        reservation.setReservableRoom(reservableRoom);
        return reservationPort.save(reservation);
    }

    @Override
    @PreAuthorize("hasRole('ADMIN') or #reservation.user.userId == principal.user.userId")
    public void cancel(Integer reservationId, User user) {
        Reservation reservation = reservationPort.findById(reservationId);
        if (reservation == null) {
            throw new ReservationNotFoundException("予約が見つかりません。");
        }
        reservationPort.delete(reservationId);
    }
}