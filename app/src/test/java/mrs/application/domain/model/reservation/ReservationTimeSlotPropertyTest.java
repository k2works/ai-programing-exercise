package mrs.application.domain.model.reservation;

import net.jqwik.api.*;
import net.jqwik.time.api.Times;

import java.time.LocalTime;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Feature: meeting-room-reservation-system, Property 26: 時間枠重複検出アルゴリズム
 * 検証: 要件 10.4
 */
class ReservationTimeSlotPropertyTest {

    @Property
    @Label("プロパティ26: 時間枠重複検出アルゴリズム - 重複する時間帯は必ず検出される")
    void overlappingTimeSlotsAreAlwaysDetected(
        @ForAll("validTimeSlot") ReservationTimeSlot slot1,
        @ForAll("overlappingTimeSlot") ReservationTimeSlot slot2
    ) {
        // 時間帯が重複している場合、overlap()はtrueを返す
        if (slot1.startTime().isBefore(slot2.endTime()) && 
            slot2.startTime().isBefore(slot1.endTime())) {
            assertTrue(slot1.overlap(slot2), 
                "重複する時間帯: " + slot1 + " と " + slot2);
        }
    }

    @Property
    @Label("プロパティ26: 時間枠重複検出アルゴリズム - 重複しない時間帯は検出されない")
    void nonOverlappingTimeSlotsAreNotDetected(
        @ForAll("validTimeSlot") ReservationTimeSlot slot1
    ) {
        // 連続する時間帯（終了時刻 = 開始時刻）は重複しない
        ReservationTimeSlot adjacentSlot = new ReservationTimeSlot(
            slot1.endTime(),
            slot1.endTime().plusHours(1)
        );
        
        assertFalse(slot1.overlap(adjacentSlot),
            "連続する時間帯は重複しない: " + slot1 + " と " + adjacentSlot);
    }

    @Property
    @Label("プロパティ9: 30分単位時刻の検証")
    void timeSlotsMustBeIn30MinuteUnits(
        @ForAll("validTimeSlot") ReservationTimeSlot slot
    ) {
        // 開始時刻と終了時刻は30分単位
        assertTrue(slot.startTime().getMinute() == 0 || slot.startTime().getMinute() == 30,
            "開始時刻は30分単位: " + slot.startTime());
        assertTrue(slot.endTime().getMinute() == 0 || slot.endTime().getMinute() == 30,
            "終了時刻は30分単位: " + slot.endTime());
        assertEquals(0, slot.startTime().getSecond());
        assertEquals(0, slot.endTime().getSecond());
    }

    @Provide
    Arbitrary<ReservationTimeSlot> validTimeSlot() {
        Arbitrary<LocalTime> startTimes = Arbitraries.of(
            LocalTime.of(9, 0), LocalTime.of(9, 30),
            LocalTime.of(10, 0), LocalTime.of(10, 30),
            LocalTime.of(11, 0), LocalTime.of(11, 30),
            LocalTime.of(12, 0), LocalTime.of(12, 30),
            LocalTime.of(13, 0), LocalTime.of(13, 30),
            LocalTime.of(14, 0), LocalTime.of(14, 30),
            LocalTime.of(15, 0), LocalTime.of(15, 30),
            LocalTime.of(16, 0), LocalTime.of(16, 30),
            LocalTime.of(17, 0), LocalTime.of(17, 30)
        );
        
        return startTimes.flatMap(start -> {
            Arbitrary<LocalTime> endTimes = Arbitraries.of(
                LocalTime.of(9, 30), LocalTime.of(10, 0), LocalTime.of(10, 30),
                LocalTime.of(11, 0), LocalTime.of(11, 30), LocalTime.of(12, 0),
                LocalTime.of(12, 30), LocalTime.of(13, 0), LocalTime.of(13, 30),
                LocalTime.of(14, 0), LocalTime.of(14, 30), LocalTime.of(15, 0),
                LocalTime.of(15, 30), LocalTime.of(16, 0), LocalTime.of(16, 30),
                LocalTime.of(17, 0), LocalTime.of(17, 30), LocalTime.of(18, 0)
            ).filter(end -> end.isAfter(start));
            
            return endTimes.map(end -> new ReservationTimeSlot(start, end));
        });
    }

    @Provide
    Arbitrary<ReservationTimeSlot> overlappingTimeSlot() {
        return validTimeSlot();
    }
}
