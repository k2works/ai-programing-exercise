package mrs.infrastructure.out.persistence;

import jakarta.persistence.LockModeType;
import mrs.infrastructure.out.persistence.entity.ReservableRoomEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Lock;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.time.LocalDate;
import java.util.List;

public interface ReservableRoomJpaRepository extends JpaRepository<ReservableRoomEntity, ReservableRoomEntity.ReservableRoomId> {
    
    @Lock(LockModeType.PESSIMISTIC_WRITE)
    @Query("SELECT r FROM ReservableRoomEntity r WHERE r.reservedDate = :reservedDate AND r.roomId = :roomId")
    ReservableRoomEntity findOneForUpdate(@Param("reservedDate") LocalDate reservedDate, @Param("roomId") Integer roomId);
    
    List<ReservableRoomEntity> findByReservedDateOrderByRoomIdAsc(LocalDate reservedDate);
}
