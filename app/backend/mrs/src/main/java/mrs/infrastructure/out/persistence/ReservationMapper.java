package mrs.infrastructure.out.persistence;

import mrs.application.domain.model.reservation.Reservation;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.time.LocalDate;
import java.util.List;

@Mapper
public interface ReservationMapper {
    List<Reservation> findByRoomIdAndDate(@Param("roomId") Integer roomId, @Param("date") LocalDate date);
    Reservation findById(@Param("reservationId") Integer reservationId);
    void insert(Reservation reservation);
    void deleteById(@Param("reservationId") Integer reservationId);
}