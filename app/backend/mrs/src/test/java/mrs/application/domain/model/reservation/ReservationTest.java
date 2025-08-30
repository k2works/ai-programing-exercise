package mrs.application.domain.model.reservation;

import mrs.application.domain.model.auth.User;
import mrs.application.domain.model.room.MeetingRoom;
import mrs.application.domain.model.room.ReservableRoom;
import org.junit.jupiter.api.Test;

import java.time.LocalDate;
import java.time.LocalTime;

import static org.junit.jupiter.api.Assertions.*;

class ReservationTest {

    @Test
    void testGettersAndSetters() {
        // テストデータ準備
        Reservation reservation = new Reservation();
        
        ReservableRoom reservableRoom = new ReservableRoom();
        reservableRoom.setRoomId(1);
        reservableRoom.setReservableDate(LocalDate.of(2025, 1, 1));
        
        User user = new User();
        user.setUserId("user001");
        user.setName("田中太郎");
        user.setPasswordHash("hashedPassword");
        user.setRole("USER");
        
        LocalTime startTime = LocalTime.of(10, 0);
        LocalTime endTime = LocalTime.of(12, 0);
        
        // setter テスト
        reservation.setReservationId(100);
        reservation.setStartTime(startTime);
        reservation.setEndTime(endTime);
        reservation.setReservableRoom(reservableRoom);
        reservation.setUser(user);
        
        // getter テスト
        assertEquals(100, reservation.getReservationId());
        assertEquals(startTime, reservation.getStartTime());
        assertEquals(endTime, reservation.getEndTime());
        assertEquals(reservableRoom, reservation.getReservableRoom());
        assertEquals(user, reservation.getUser());
    }

    @Test
    void testOverlap_重複する場合() {
        // 10:00-12:00 の予約
        Reservation reservation1 = createReservation(LocalTime.of(10, 0), LocalTime.of(12, 0));
        
        // 11:00-13:00 の予約（重複）
        Reservation reservation2 = createReservation(LocalTime.of(11, 0), LocalTime.of(13, 0));
        
        assertTrue(reservation1.overlap(reservation2));
        assertTrue(reservation2.overlap(reservation1));
    }

    @Test
    void testOverlap_重複しない場合_前の時間帯() {
        // 10:00-11:00 の予約
        Reservation reservation1 = createReservation(LocalTime.of(10, 0), LocalTime.of(11, 0));
        
        // 11:00-12:00 の予約（重複しない）
        Reservation reservation2 = createReservation(LocalTime.of(11, 0), LocalTime.of(12, 0));
        
        assertFalse(reservation1.overlap(reservation2));
        assertFalse(reservation2.overlap(reservation1));
    }

    @Test
    void testOverlap_重複しない場合_後の時間帯() {
        // 13:00-14:00 の予約
        Reservation reservation1 = createReservation(LocalTime.of(13, 0), LocalTime.of(14, 0));
        
        // 10:00-12:00 の予約（重複しない）
        Reservation reservation2 = createReservation(LocalTime.of(10, 0), LocalTime.of(12, 0));
        
        assertFalse(reservation1.overlap(reservation2));
        assertFalse(reservation2.overlap(reservation1));
    }

    @Test
    void testOverlap_完全に内包する場合() {
        // 9:00-15:00 の予約
        Reservation reservation1 = createReservation(LocalTime.of(9, 0), LocalTime.of(15, 0));
        
        // 10:00-12:00 の予約（内包される）
        Reservation reservation2 = createReservation(LocalTime.of(10, 0), LocalTime.of(12, 0));
        
        assertTrue(reservation1.overlap(reservation2));
        assertTrue(reservation2.overlap(reservation1));
    }

    @Test
    void testOverlap_同じ時間帯() {
        // 10:00-12:00 の予約
        Reservation reservation1 = createReservation(LocalTime.of(10, 0), LocalTime.of(12, 0));
        
        // 10:00-12:00 の予約（同じ）
        Reservation reservation2 = createReservation(LocalTime.of(10, 0), LocalTime.of(12, 0));
        
        assertTrue(reservation1.overlap(reservation2));
        assertTrue(reservation2.overlap(reservation1));
    }

    @Test
    void testOverlap_nullの場合() {
        Reservation reservation = createReservation(LocalTime.of(10, 0), LocalTime.of(12, 0));
        
        assertFalse(reservation.overlap(null));
    }

    @Test
    void testOverlap_開始時間が同じで終了時間が異なる場合() {
        // 10:00-11:00 の予約
        Reservation reservation1 = createReservation(LocalTime.of(10, 0), LocalTime.of(11, 0));
        
        // 10:00-12:00 の予約（重複）
        Reservation reservation2 = createReservation(LocalTime.of(10, 0), LocalTime.of(12, 0));
        
        assertTrue(reservation1.overlap(reservation2));
        assertTrue(reservation2.overlap(reservation1));
    }

    private Reservation createReservation(LocalTime startTime, LocalTime endTime) {
        Reservation reservation = new Reservation();
        reservation.setStartTime(startTime);
        reservation.setEndTime(endTime);
        return reservation;
    }
}