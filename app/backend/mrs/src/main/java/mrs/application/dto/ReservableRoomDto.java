package mrs.application.dto;

import java.time.LocalDate;

/**
 * 予約可能会議室DTO
 * Web層で予約可能会議室情報を表現するためのデータ転送オブジェクト
 */
public class ReservableRoomDto {
    private Integer roomId;
    private LocalDate reservableDate;

    public ReservableRoomDto() {
    }

    public ReservableRoomDto(Integer roomId, LocalDate reservableDate) {
        this.roomId = roomId;
        this.reservableDate = reservableDate;
    }

    public Integer getRoomId() {
        return roomId;
    }

    public void setRoomId(Integer roomId) {
        this.roomId = roomId;
    }

    public LocalDate getReservableDate() {
        return reservableDate;
    }

    public void setReservableDate(LocalDate reservableDate) {
        this.reservableDate = reservableDate;
    }
}