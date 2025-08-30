package mrs.application.domain.model.room;

import java.time.LocalDate;

public class ReservableRoom {
    private Integer roomId;
    private LocalDate reservableDate;

    public Integer getRoomId() { return roomId; }
    public void setRoomId(Integer roomId) { this.roomId = roomId; }

    public LocalDate getReservableDate() { return reservableDate; }
    public void setReservableDate(LocalDate reservableDate) { this.reservableDate = reservableDate; }
}