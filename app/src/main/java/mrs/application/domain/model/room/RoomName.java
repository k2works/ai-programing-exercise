package mrs.application.domain.model.room;

public record RoomName(String value) {
    public RoomName {
        if (value == null || value.isEmpty()) {
            throw new IllegalArgumentException("Room name cannot be null or empty");
        }
    }
}
