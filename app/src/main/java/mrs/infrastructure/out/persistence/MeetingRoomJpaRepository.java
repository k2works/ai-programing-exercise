package mrs.infrastructure.out.persistence;

import mrs.infrastructure.out.persistence.entity.MeetingRoomEntity;
import org.springframework.data.jpa.repository.JpaRepository;

public interface MeetingRoomJpaRepository extends JpaRepository<MeetingRoomEntity, Integer> {
}
