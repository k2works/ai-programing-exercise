package mrs;

import mrs.application.domain.model.auth.*;
import mrs.application.domain.model.reservation.*;
import mrs.application.domain.model.room.*;
import mrs.application.service.reservation.ReservationService;
import mrs.application.service.room.RoomService;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.ActiveProfiles;

import java.time.LocalDate;
import java.time.LocalTime;

import static org.junit.jupiter.api.Assertions.*;

/**
 * 統合テスト: アプリケーション全体の動作確認
 */
@SpringBootTest
@ActiveProfiles("test")
class IntegrationTest {

    @Autowired
    private ReservationService reservationService;

    @Autowired
    private RoomService roomService;

    @Test
    void アプリケーションコンテキストが正常にロードされる() {
        assertNotNull(reservationService);
        assertNotNull(roomService);
    }

    @Test
    void 予約可能会議室一覧を取得できる() {
        LocalDate tomorrow = LocalDate.now().plusDays(1);
        ReservedDate reservedDate = new ReservedDate(tomorrow);
        
        ReservableRoomList rooms = roomService.findReservableRooms(reservedDate);
        
        assertNotNull(rooms);
        assertTrue(rooms.size() > 0, "予約可能な会議室が存在すること");
    }

    @Test
    void 予約の重複判定が正しく動作する() {
        Reservation reservation1 = new Reservation(
            new ReservationId(1),
            new ReservationTimeSlot(LocalTime.of(9, 0), LocalTime.of(10, 0)),
            new ReservableRoomId(LocalDate.now(), new RoomId(1)),
            new UserId("user001")
        );

        Reservation reservation2 = new Reservation(
            new ReservationId(2),
            new ReservationTimeSlot(LocalTime.of(9, 30), LocalTime.of(10, 30)),
            new ReservableRoomId(LocalDate.now(), new RoomId(1)),
            new UserId("user002")
        );

        assertTrue(reservation1.overlap(reservation2), "重複が検出されること");
    }

    @Test
    void ユーザーの権限判定が正しく動作する() {
        User admin = new User(
            new UserId("admin"),
            new Password("password"),
            new Name("管理", "者"),
            RoleName.ADMIN
        );

        User user = new User(
            new UserId("user"),
            new Password("password"),
            new Name("一般", "ユーザー"),
            RoleName.USER
        );

        assertTrue(admin.isAdmin(), "管理者権限を持つこと");
        assertFalse(user.isAdmin(), "一般ユーザーは管理者権限を持たないこと");
    }

    @Test
    void キャンセル権限の判定が正しく動作する() {
        Reservation reservation = new Reservation(
            new ReservationId(1),
            new ReservationTimeSlot(LocalTime.of(9, 0), LocalTime.of(10, 0)),
            new ReservableRoomId(LocalDate.now(), new RoomId(1)),
            new UserId("user001")
        );

        User owner = new User(
            new UserId("user001"),
            new Password("password"),
            new Name("予約", "者"),
            RoleName.USER
        );

        User other = new User(
            new UserId("user002"),
            new Password("password"),
            new Name("他", "ユーザー"),
            RoleName.USER
        );

        User admin = new User(
            new UserId("admin"),
            new Password("password"),
            new Name("管理", "者"),
            RoleName.ADMIN
        );

        assertTrue(reservationService.canCancel(reservation, owner), "予約者はキャンセル可能");
        assertFalse(reservationService.canCancel(reservation, other), "他人はキャンセル不可");
        assertTrue(reservationService.canCancel(reservation, admin), "管理者はキャンセル可能");
    }
}
