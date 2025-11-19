package mrs.infrastructure.out.persistence.entity;

import jakarta.persistence.*;
import java.time.LocalDate;
import java.time.LocalTime;

@Entity
@Table(name = "reservation")
public class ReservationEntity {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "reservation_id")
    private Integer reservationId;

    @Column(name = "start_time", nullable = false)
    private LocalTime startTime;

    @Column(name = "end_time", nullable = false)
    private LocalTime endTime;

    @Column(name = "reserved_date", nullable = false)
    private LocalDate reservedDate;

    @Column(name = "room_id", nullable = false)
    private Integer roomId;

    @Column(name = "user_id", nullable = false)
    private String userId;

    @ManyToOne
    @JoinColumns({
        @JoinColumn(name = "reserved_date", referencedColumnName = "reserved_date", insertable = false, updatable = false),
        @JoinColumn(name = "room_id", referencedColumnName = "room_id", insertable = false, updatable = false)
    })
    private ReservableRoomEntity reservableRoom;

    @ManyToOne
    @JoinColumn(name = "user_id", insertable = false, updatable = false)
    private UserEntity user;

    public ReservationEntity() {
    }

    public ReservationEntity(Integer reservationId, LocalTime startTime, LocalTime endTime, 
                            LocalDate reservedDate, Integer roomId, String userId) {
        this.reservationId = reservationId;
        this.startTime = startTime;
        this.endTime = endTime;
        this.reservedDate = reservedDate;
        this.roomId = roomId;
        this.userId = userId;
    }

    public Integer getReservationId() {
        return reservationId;
    }

    public void setReservationId(Integer reservationId) {
        this.reservationId = reservationId;
    }

    public LocalTime getStartTime() {
        return startTime;
    }

    public void setStartTime(LocalTime startTime) {
        this.startTime = startTime;
    }

    public LocalTime getEndTime() {
        return endTime;
    }

    public void setEndTime(LocalTime endTime) {
        this.endTime = endTime;
    }

    public LocalDate getReservedDate() {
        return reservedDate;
    }

    public void setReservedDate(LocalDate reservedDate) {
        this.reservedDate = reservedDate;
    }

    public Integer getRoomId() {
        return roomId;
    }

    public void setRoomId(Integer roomId) {
        this.roomId = roomId;
    }

    public String getUserId() {
        return userId;
    }

    public void setUserId(String userId) {
        this.userId = userId;
    }

    public ReservableRoomEntity getReservableRoom() {
        return reservableRoom;
    }

    public void setReservableRoom(ReservableRoomEntity reservableRoom) {
        this.reservableRoom = reservableRoom;
    }

    public UserEntity getUser() {
        return user;
    }

    public void setUser(UserEntity user) {
        this.user = user;
    }
}
