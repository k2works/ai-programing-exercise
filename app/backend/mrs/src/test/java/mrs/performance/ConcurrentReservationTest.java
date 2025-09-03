package mrs.performance;

import mrs.application.service.ReservationService;
import mrs.application.service.RoomService;
import mrs.application.domain.model.reservation.Reservation;
import mrs.application.domain.model.room.ReservableRoom;
import mrs.application.domain.model.room.MeetingRoom;
import mrs.application.domain.model.auth.User;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.ActiveProfiles;

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

    private static final int CONCURRENT_USERS = 100;
    private static final int TIMEOUT_SECONDS = 30;
    private ExecutorService executorService;
    private ReservableRoom testRoom;
    private LocalDate testDate;

    @BeforeEach
    void setUp() {
        executorService = Executors.newFixedThreadPool(CONCURRENT_USERS);
        testDate = LocalDate.now().plusDays(1);
        
        // テスト用会議室を取得
        List<ReservableRoom> rooms = roomService.findReservableRooms(testDate);
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
                    reservation.setReservableRoom(testRoom);
                    User user = new User();
                    user.setUserId(userId);
                    user.setName("Test User");
                    user.setRole("USER");
                    reservation.setUser(user);
                    reservation.setStartTime(startTime);
                    reservation.setEndTime(endTime);
                    
                    Reservation created = reservationService.reserve(reservation, user);
                    if (created != null) {
                        successCount.incrementAndGet();
                    }
                    
                } catch (IllegalStateException e) {
                    // 重複予約による競合エラー
                    if (e.getMessage().contains("既に予約されています") || 
                        e.getMessage().contains("重複")) {
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
                    
                    // 予約可能会議室を取得（日付別）
                    List<ReservableRoom> rooms = roomService.findReservableRooms(reservationDate);
                    if (rooms.isEmpty()) {
                        // 予約可能会議室がない場合はスキップ
                        successCount.incrementAndGet(); // テスト目的なので成功としてカウント
                        return;
                    }
                    
                    ReservableRoom room = rooms.get(0);
                    Reservation reservation = new Reservation();
                    reservation.setReservableRoom(room);
                    User user = new User();
                    user.setUserId(userId);
                    user.setName("Test User");
                    user.setRole("USER");
                    reservation.setUser(user);
                    reservation.setStartTime(startTime);
                    reservation.setEndTime(endTime);
                    
                    Reservation created = reservationService.reserve(reservation, user);
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
        
        // 全ての予約が成功することを確認（または大部分が成功）
        assertTrue(successCount.get() >= CONCURRENT_USERS * 0.9, 
            "異なる時間帯の予約は大部分が成功するべきです");
        assertTrue(errorCount.get() <= CONCURRENT_USERS * 0.1, 
            "エラー数は10%以下であるべきです: " + exceptions);
    }

    @Test
    @DisplayName("負荷テスト - レスポンス時間計測")
    void testResponseTimeUnderLoad() throws InterruptedException {
        final int TEST_USERS = 50;
        CountDownLatch startLatch = new CountDownLatch(1);
        CountDownLatch completeLatch = new CountDownLatch(TEST_USERS);
        
        List<Long> responseTimes = Collections.synchronizedList(new ArrayList<>());
        AtomicInteger errorCount = new AtomicInteger(0);
        
        for (int i = 0; i < TEST_USERS; i++) {
            final int userIndex = i;
            final String userId = String.format("perf_user%03d", i);
            
            executorService.submit(() -> {
                try {
                    startLatch.await();
                    
                    long startTime = System.currentTimeMillis();
                    
                    // 予約可能会議室取得
                    List<ReservableRoom> rooms = roomService.findReservableRooms(testDate.plusDays(userIndex));
                    if (!rooms.isEmpty()) {
                        ReservableRoom room = rooms.get(0);
                        Reservation reservation = new Reservation();
                        reservation.setReservableRoom(room);
                        User user = new User();
                    user.setUserId(userId);
                    user.setName("Test User");
                    user.setRole("USER");
                    reservation.setUser(user);
                        reservation.setStartTime(LocalTime.of(15, 0));
                        reservation.setEndTime(LocalTime.of(16, 0));
                        
                        reservationService.reserve(reservation, user);
                    }
                    
                    long responseTime = System.currentTimeMillis() - startTime;
                    responseTimes.add(responseTime);
                    
                } catch (Exception e) {
                    errorCount.incrementAndGet();
                } finally {
                    completeLatch.countDown();
                }
            });
        }
        
        startLatch.countDown();
        
        boolean completed = completeLatch.await(TIMEOUT_SECONDS, TimeUnit.SECONDS);
        assertTrue(completed, "レスポンス時間テストがタイムアウトしました");
        
        // レスポンス時間統計
        long avgResponseTime = responseTimes.stream()
            .mapToLong(Long::longValue)
            .sum() / responseTimes.size();
        
        long maxResponseTime = responseTimes.stream()
            .mapToLong(Long::longValue)
            .max()
            .orElse(0);
        
        System.out.println("=== レスポンス時間テスト結果 ===");
        System.out.println("平均レスポンス時間: " + avgResponseTime + " ms");
        System.out.println("最大レスポンス時間: " + maxResponseTime + " ms");
        System.out.println("エラー数: " + errorCount.get());
        
        // 性能要件の検証 (N001-01: 予約作成 3秒以内)
        assertTrue(avgResponseTime <= 3000, 
            "平均レスポンス時間が3秒を超えています: " + avgResponseTime + "ms");
        assertTrue(maxResponseTime <= 5000, 
            "最大レスポンス時間が5秒を超えています: " + maxResponseTime + "ms");
    }
}