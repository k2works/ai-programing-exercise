package mrs.infrastructure.out.persistence.entity;

import jakarta.persistence.*;
import java.io.Serializable;
import java.time.LocalDate;
import java.util.Objects;

@Entity
@Table(name = "reservable_room")
@IdClass(ReservableRoomEntity.ReservableRoomId.class)
public class ReservableRoomEntity {
    @Id
    @Column(name = "reserved_date", nullable = false)
    private LocalDate reservedDate;

    @Id
    @Column(name = "room_id", nullable = false)
    private Integer roomId;

    @ManyToOne
    @JoinColumn(name = "room_id", insertable = false, updatable = false)
    private MeetingRoomEntity meetingRoom;

    public ReservableRoomEntity() {
    }

    public ReservableRoomEntity(LocalDate reservedDate, Integer roomId) {
        this.reservedDate = reservedDate;
        this.roomId = roomId;
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

    public MeetingRoomEntity getMeetingRoom() {
        return meetingRoom;
    }

    public void setMeetingRoom(MeetingRoomEntity meetingRoom) {
        this.meetingRoom = meetingRoom;
    }

    public static class ReservableRoomId implements Serializable {
        private LocalDate reservedDate;
        private Integer roomId;

        public ReservableRoomId() {
        }

        public ReservableRoomId(LocalDate reservedDate, Integer roomId) {
            this.reservedDate = reservedDate;
            this.roomId = roomId;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            ReservableRoomId that = (ReservableRoomId) o;
            return Objects.equals(reservedDate, that.reservedDate) && Objects.equals(roomId, that.roomId);
        }

        @Override
        public int hashCode() {
            return Objects.hash(reservedDate, roomId);
        }
    }
}
