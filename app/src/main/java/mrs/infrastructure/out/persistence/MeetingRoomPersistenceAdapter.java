package mrs.infrastructure.out.persistence;

import mrs.application.domain.model.room.*;
import mrs.application.port.out.MeetingRoomPort;
import mrs.infrastructure.out.persistence.entity.MeetingRoomEntity;
import org.springframework.stereotype.Component;

import java.util.List;

@Component
public class MeetingRoomPersistenceAdapter implements MeetingRoomPort {
    private final MeetingRoomJpaRepository repository;

    public MeetingRoomPersistenceAdapter(MeetingRoomJpaRepository repository) {
        this.repository = repository;
    }

    @Override
    public MeetingRoom findById(RoomId roomId) {
        return repository.findById(roomId.value())
            .map(this::toDomain)
            .orElse(null);
    }

    @Override
    public List<MeetingRoom> findAll() {
        return repository.findAll().stream()
            .map(this::toDomain)
            .toList();
    }

    private MeetingRoom toDomain(MeetingRoomEntity entity) {
        return new MeetingRoom(
            new RoomId(entity.getRoomId()),
            new RoomName(entity.getRoomName())
        );
    }
}
