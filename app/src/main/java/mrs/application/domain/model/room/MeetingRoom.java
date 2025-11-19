package mrs.application.domain.model.room;

public record MeetingRoom(RoomId roomId, RoomName roomName) {
    public MeetingRoom {
        if (roomId == null) {
            throw new IllegalArgumentException("RoomId cannot be null");
        }
        if (roomName == null) {
            throw new IllegalArgumentException("RoomName cannot be null");
        }
    }
}
