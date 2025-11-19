package mrs.infrastructure.out.persistence;

import mrs.infrastructure.out.persistence.entity.ReservationEntity;
import org.springframework.data.jpa.repository.JpaRepository;

import java.time.LocalDate;
import java.util.List;

public interface ReservationJpaRepository extends JpaRepository<ReservationEntity, Integer> {
    List<ReservationEntity> findByReservedDateAndRoomIdOrderByStartTimeAsc(LocalDate reservedDate, Integer roomId);
}
