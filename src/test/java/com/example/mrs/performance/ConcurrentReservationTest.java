package com.example.mrs.performance;

import com.example.mrs.domain.model.Reservation;
import com.example.mrs.domain.model.Room;
import com.example.mrs.domain.model.User;
import com.example.mrs.domain.service.ReservationService;
import com.example.mrs.domain.service.RoomService;
import com.example.mrs.infrastructure.security.JwtUtil;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDate;
import java.time.LocalTime;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.*;
import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.*;

/**
 * 並行予約処理の負荷テスト
 * 100同時ユーザー相当の並行処理でデータ整合性を検証
 */
@SpringBootTest
@ActiveProfiles("test")
@DisplayName("並行予約処理テスト")
public class ConcurrentReservationTest {

    @Autowired
    private ReservationService reservationService;

    @Autowired
    private RoomService roomService;

    @Autowired
    private JwtUtil jwtUtil;

    private static final int CONCURRENT_USERS = 100;
    private static final int TIMEOUT_SECONDS = 30;
    private ExecutorService executorService;
    private Room testRoom;
    private LocalDate testDate;

    @BeforeEach
    void setUp() {
        executorService = Executors.newFixedThreadPool(CONCURRENT_USERS);
        testDate = LocalDate.now().plusDays(1);
        
        // テスト用会議室を取得
        List<Room> rooms = roomService.findAll();
        assertFalse(rooms.isEmpty(), "テスト用会議室が存在しません");
        testRoom = rooms.get(0);
    }

    @Test
    @DisplayName("100ユーザーが同時に同じ時間帯を予約した場合、1件のみ成功する")
    void testConcurrentReservationForSameTimeSlot() throws InterruptedException {
        final LocalTime startTime = LocalTime.of(10, 0);
        final LocalTime endTime = LocalTime.of(11, 0);
        
        CountDownLatch startLatch = new CountDownLatch(1);
        CountDownLatch completeLatch = new CountDownLatch(CONCURRENT_USERS);
        
        AtomicInteger successCount = new AtomicInteger(0);
        AtomicInteger conflictCount = new AtomicInteger(0);
        AtomicInteger errorCount = new AtomicInteger(0);
        List<Exception> exceptions = Collections.synchronizedList(new ArrayList<>());
        
        // 100ユーザーの並行予約実行
        for (int i = 1; i <= CONCURRENT_USERS; i++) {
            final String userId = String.format("user%03d", i);
            
            executorService.submit(() -> {
                try {
                    // 全スレッドが同時にスタートするよう待機
                    startLatch.await();
                    
                    // 予約作成を試行
                    Reservation reservation = new Reservation();
                    reservation.setRoomId(testRoom.getRoomId());
                    reservation.setUserId(userId);
                    reservation.setReservableDate(testDate);
                    reservation.setStartTime(startTime);
                    reservation.setEndTime(endTime);
                    
                    Reservation created = reservationService.create(reservation, userId);
                    if (created != null) {
                        successCount.incrementAndGet();
                    }
                    
                } catch (IllegalStateException e) {
                    // 重複予約による競合エラー
                    if (e.getMessage().contains("既に予約が存在") || 
                        e.getMessage().contains("時間帯が重複")) {
                        conflictCount.incrementAndGet();
                    } else {
                        errorCount.incrementAndGet();
                        exceptions.add(e);
                    }
                } catch (Exception e) {
                    errorCount.incrementAndGet();
                    exceptions.add(e);
                } finally {
                    completeLatch.countDown();
                }
            });
        }
        
        // 全スレッドを同時にスタート
        startLatch.countDown();
        
        // 完了を待機（タイムアウト付き）
        boolean completed = completeLatch.await(TIMEOUT_SECONDS, TimeUnit.SECONDS);
        assertTrue(completed, "テストがタイムアウトしました");
        
        // 結果の検証
        System.out.println("=== 並行予約テスト結果 ===");
        System.out.println("成功数: " + successCount.get());
        System.out.println("競合数: " + conflictCount.get());
        System.out.println("エラー数: " + errorCount.get());
        
        // 成功は1件のみであることを確認
        assertEquals(1, successCount.get(), 
            "同一時間帯の予約は1件のみ成功するべきです");
        
        // 残りは全て競合エラーであることを確認
        assertEquals(CONCURRENT_USERS - 1, conflictCount.get(), 
            "他の予約は全て競合エラーになるべきです");
        
        // 予期しないエラーがないことを確認
        assertEquals(0, errorCount.get(), 
            "予期しないエラーが発生しました: " + exceptions);
    }

    @Test
    @DisplayName("100ユーザーが異なる時間帯を予約した場合、全て成功する")
    void testConcurrentReservationForDifferentTimeSlots() throws InterruptedException {
        CountDownLatch startLatch = new CountDownLatch(1);
        CountDownLatch completeLatch = new CountDownLatch(CONCURRENT_USERS);
        
        AtomicInteger successCount = new AtomicInteger(0);
        AtomicInteger errorCount = new AtomicInteger(0);
        List<Exception> exceptions = Collections.synchronizedList(new ArrayList<>());
        
        // 各ユーザーに異なる時間帯を割り当て（30分刻み）
        for (int i = 0; i < CONCURRENT_USERS; i++) {
            final int userIndex = i;
            final String userId = String.format("user%03d", i + 1);
            
            // 日付を分散（10日間）
            final LocalDate reservationDate = testDate.plusDays(userIndex / 10);
            // 時間を分散（9:00-18:00の間で30分刻み）
            final int slotIndex = userIndex % 10;
            final LocalTime startTime = LocalTime.of(9, 0).plusMinutes(slotIndex * 30);
            final LocalTime endTime = startTime.plusMinutes(30);
            
            executorService.submit(() -> {
                try {
                    startLatch.await();
                    
                    Reservation reservation = new Reservation();
                    reservation.setRoomId(testRoom.getRoomId());
                    reservation.setUserId(userId);
                    reservation.setReservableDate(reservationDate);
                    reservation.setStartTime(startTime);
                    reservation.setEndTime(endTime);
                    
                    Reservation created = reservationService.create(reservation, userId);
                    if (created != null) {
                        successCount.incrementAndGet();
                    }
                    
                } catch (Exception e) {
                    errorCount.incrementAndGet();
                    exceptions.add(e);
                } finally {
                    completeLatch.countDown();
                }
            });
        }
        
        startLatch.countDown();
        
        boolean completed = completeLatch.await(TIMEOUT_SECONDS, TimeUnit.SECONDS);
        assertTrue(completed, "テストがタイムアウトしました");
        
        System.out.println("=== 異なる時間帯の並行予約テスト結果 ===");
        System.out.println("成功数: " + successCount.get());
        System.out.println("エラー数: " + errorCount.get());
        
        // 全ての予約が成功することを確認
        assertEquals(CONCURRENT_USERS, successCount.get(), 
            "異なる時間帯の予約は全て成功するべきです");
        assertEquals(0, errorCount.get(), 
            "エラーが発生しました: " + exceptions);
    }

    @Test
    @DisplayName("デッドロック検出テスト - 複数会議室の交差予約")
    @Transactional
    void testDeadlockDetection() throws InterruptedException {
        List<Room> rooms = roomService.findAll();
        assertTrue(rooms.size() >= 2, "デッドロックテストには2つ以上の会議室が必要です");
        
        Room room1 = rooms.get(0);
        Room room2 = rooms.get(1);
        
        final int THREADS = 10;
        CountDownLatch startLatch = new CountDownLatch(1);
        CountDownLatch completeLatch = new CountDownLatch(THREADS * 2);
        
        AtomicInteger deadlockCount = new AtomicInteger(0);
        AtomicInteger successCount = new AtomicInteger(0);
        
        // グループA: room1 → room2の順で予約
        for (int i = 0; i < THREADS; i++) {
            final String userId = "groupA_user" + i;
            executorService.submit(() -> {
                try {
                    startLatch.await();
                    
                    // room1を予約
                    Reservation res1 = createReservation(room1, userId, 
                        LocalTime.of(14, 0), LocalTime.of(15, 0));
                    reservationService.create(res1, userId);
                    
                    Thread.sleep(10); // わざと遅延を入れる
                    
                    // room2を予約
                    Reservation res2 = createReservation(room2, userId, 
                        LocalTime.of(14, 0), LocalTime.of(15, 0));
                    reservationService.create(res2, userId);
                    
                    successCount.incrementAndGet();
                    
                } catch (Exception e) {
                    if (e.getMessage() != null && 
                        (e.getMessage().contains("deadlock") || 
                         e.getMessage().contains("lock timeout"))) {
                        deadlockCount.incrementAndGet();
                    }
                } finally {
                    completeLatch.countDown();
                }
            });
        }
        
        // グループB: room2 → room1の順で予約（逆順）
        for (int i = 0; i < THREADS; i++) {
            final String userId = "groupB_user" + i;
            executorService.submit(() -> {
                try {
                    startLatch.await();
                    
                    // room2を予約
                    Reservation res2 = createReservation(room2, userId, 
                        LocalTime.of(15, 0), LocalTime.of(16, 0));
                    reservationService.create(res2, userId);
                    
                    Thread.sleep(10); // わざと遅延を入れる
                    
                    // room1を予約
                    Reservation res1 = createReservation(room1, userId, 
                        LocalTime.of(15, 0), LocalTime.of(16, 0));
                    reservationService.create(res1, userId);
                    
                    successCount.incrementAndGet();
                    
                } catch (Exception e) {
                    if (e.getMessage() != null && 
                        (e.getMessage().contains("deadlock") || 
                         e.getMessage().contains("lock timeout"))) {
                        deadlockCount.incrementAndGet();
                    }
                } finally {
                    completeLatch.countDown();
                }
            });
        }
        
        startLatch.countDown();
        
        boolean completed = completeLatch.await(TIMEOUT_SECONDS, TimeUnit.SECONDS);
        assertTrue(completed, "デッドロックテストがタイムアウトしました");
        
        System.out.println("=== デッドロック検出テスト結果 ===");
        System.out.println("成功数: " + successCount.get());
        System.out.println("デッドロック検出数: " + deadlockCount.get());
        
        // デッドロックが適切に処理されていることを確認
        assertTrue(deadlockCount.get() == 0 || successCount.get() > 0, 
            "デッドロックが発生した場合でも、一部のトランザクションは成功するべきです");
    }

    private Reservation createReservation(Room room, String userId, 
                                         LocalTime startTime, LocalTime endTime) {
        Reservation reservation = new Reservation();
        reservation.setRoomId(room.getRoomId());
        reservation.setUserId(userId);
        reservation.setReservableDate(testDate);
        reservation.setStartTime(startTime);
        reservation.setEndTime(endTime);
        return reservation;
    }
}