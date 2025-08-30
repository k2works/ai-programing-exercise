package mrs.application.domain.model.reservation;

import edu.umd.cs.findbugs.annotations.SuppressFBWarnings;
import mrs.application.domain.model.auth.User;
import mrs.application.domain.model.room.ReservableRoom;
import java.time.LocalTime;

public class Reservation {
    private Integer reservationId;
    private LocalTime startTime;
    private LocalTime endTime;
    private ReservableRoom reservableRoom;
    private User user;

    public Integer getReservationId() { return reservationId; }
    public void setReservationId(Integer reservationId) { this.reservationId = reservationId; }

    public LocalTime getStartTime() { return startTime; }
    public void setStartTime(LocalTime startTime) { this.startTime = startTime; }

    public LocalTime getEndTime() { return endTime; }
    public void setEndTime(LocalTime endTime) { this.endTime = endTime; }

    @SuppressFBWarnings(value = "EI_EXPOSE_REP",
            justification = "Reservation is a domain model entity that appropriately exposes its associations")
    public ReservableRoom getReservableRoom() { return reservableRoom; }
    
    @SuppressFBWarnings(value = "EI_EXPOSE_REP2",
            justification = "Reservation is a domain model entity that appropriately manages object references")
    public void setReservableRoom(ReservableRoom reservableRoom) { this.reservableRoom = reservableRoom; }

    @SuppressFBWarnings(value = "EI_EXPOSE_REP",
            justification = "Reservation is a domain model entity that appropriately exposes its associations")
    public User getUser() { return user; }
    
    @SuppressFBWarnings(value = "EI_EXPOSE_REP2",
            justification = "Reservation is a domain model entity that appropriately manages object references")
    public void setUser(User user) { this.user = user; }
    
    // 重複判定メソッド
    public boolean overlap(Reservation target) {
        if (target == null) {
            return false;
        }
        // target.endTime > this.startTime && this.endTime > target.startTime
        return target.getEndTime().isAfter(this.startTime) && 
               this.endTime.isAfter(target.getStartTime());
    }
}