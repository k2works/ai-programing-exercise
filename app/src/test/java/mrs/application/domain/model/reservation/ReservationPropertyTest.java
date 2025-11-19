package mrs.application.domain.model.reservation;

import mrs.application.domain.model.auth.UserId;
import mrs.application.domain.model.room.RoomId;
import net.jqwik.api.*;
import net.jqwik.time.api.Dates;
import net.jqwik.time.api.Times;

import java.time.LocalDate;
import java.time.LocalTime;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Feature: meeting-room-reservation-system, Property 7: 予約重複の検出と拒否
 * 検証: 要件 3.2
 */
class ReservationPropertyTest {

    @Property
    @Label("プロパティ7: 同じ会議室の重複する予約は検出される")
    void overlappingReservationsInSameRoomAreDetected(
        @ForAll("reservation") Reservation reservation1,
        @ForAll("overlappingReservation") Reservation reservation2
    ) {
        // 同じ会議室で時間が重複する場合
        if (reservation1.reservableRoomId().equals(reservation2.reservableRoomId()) &&
            reservation1.timeSlot().overlap(reservation2.timeSlot())) {
            assertTrue(reservation1.overlap(reservation2),
                "重複する予約: " + reservation1 + " と " + reservation2);
        }
    }

    @Property
    @Label("プロパティ7: 異なる会議室の予約は重複しない")
    void reservationsInDifferentRoomsDoNotOverlap(
        @ForAll("reservation") Reservation reservation1
    ) {
        // 異なる会議室の予約
        ReservableRoomId differentRoom = new ReservableRoomId(
            reservation1.reservableRoomId().reservedDate(),
            new RoomId(reservation1.reservableRoomId().roomId().value() + 1)
        );
        
        Reservation reservation2 = new Reservation(
            new ReservationId(2),
            reservation1.timeSlot(),
            differentRoom,
            new UserId("user002")
        );
        
        assertFalse(reservation1.overlap(reservation2),
            "異なる会議室の予約は重複しない");
    }

    @Property
    @Label("プロパティ23: 終了時刻は開始時刻より後でなければならない")
    void endTimeMustBeAfterStartTime(
        @ForAll("validStartTime") LocalTime startTime,
        @ForAll("validEndTime") LocalTime endTime
    ) {
        Assume.that(endTime.isAfter(startTime));
        
        ReservationTimeSlot slot = new ReservationTimeSlot(startTime, endTime);
        assertTrue(slot.endTime().isAfter(slot.startTime()),
            "終了時刻は開始時刻より後: " + slot);
    }

    @Provide
    Arbitrary<Reservation> reservation() {
        return Combinators.combine(
            Arbitraries.integers().between(1, 1000),
            validTimeSlot(),
            reservableRoomId(),
            userId()
        ).as((id, timeSlot, roomId, userId) -> 
            new Reservation(new ReservationId(id), timeSlot, roomId, userId)
        );
    }

    @Provide
    Arbitrary<Reservation> overlappingReservation() {
        return reservation();
    }

    @Provide
    Arbitrary<ReservationTimeSlot> validTimeSlot() {
        return Times.times()
            .between(LocalTime.of(9, 0), LocalTime.of(17, 30))
            .filter(time -> time.getMinute() % 30 == 0)
            .flatMap(start -> Times.times()
                .between(start.plusMinutes(30), LocalTime.of(18, 0))
                .filter(time -> time.getMinute() % 30 == 0)
                .map(end -> new ReservationTimeSlot(start, end))
            );
    }

    @Provide
    Arbitrary<ReservableRoomId> reservableRoomId() {
        return Combinators.combine(
            Dates.dates().between(LocalDate.now(), LocalDate.now().plusDays(30)),
            Arbitraries.integers().between(1, 10)
        ).as((date, roomId) -> new ReservableRoomId(date, new RoomId(roomId)));
    }

    @Provide
    Arbitrary<UserId> userId() {
        return Arbitraries.strings()
            .alpha()
            .ofMinLength(5)
            .ofMaxLength(10)
            .map(UserId::new);
    }

    @Provide
    Arbitrary<LocalTime> validStartTime() {
        return Times.times()
            .between(LocalTime.of(9, 0), LocalTime.of(17, 30))
            .filter(time -> time.getMinute() % 30 == 0);
    }

    @Provide
    Arbitrary<LocalTime> validEndTime() {
        return Times.times()
            .between(LocalTime.of(9, 30), LocalTime.of(18, 0))
            .filter(time -> time.getMinute() % 30 == 0);
    }
}
