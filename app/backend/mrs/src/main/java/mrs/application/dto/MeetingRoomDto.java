package mrs.application.dto;

/**
 * 会議室DTO
 * Web層で会議室情報を表現するためのデータ転送オブジェクト
 */
public class MeetingRoomDto {
    private Integer roomId;
    private String roomName;

    public MeetingRoomDto() {
    }

    public MeetingRoomDto(Integer roomId, String roomName) {
        this.roomId = roomId;
        this.roomName = roomName;
    }

    public Integer getRoomId() {
        return roomId;
    }

    public void setRoomId(Integer roomId) {
        this.roomId = roomId;
    }

    public String getRoomName() {
        return roomName;
    }

    public void setRoomName(String roomName) {
        this.roomName = roomName;
    }
}