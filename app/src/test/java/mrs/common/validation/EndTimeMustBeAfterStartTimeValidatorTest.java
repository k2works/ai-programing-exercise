package mrs.common.validation;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.time.LocalTime;

import static org.junit.jupiter.api.Assertions.*;

class EndTimeMustBeAfterStartTimeValidatorTest {
    private EndTimeMustBeAfterStartTimeValidator validator;

    @BeforeEach
    void setUp() {
        validator = new EndTimeMustBeAfterStartTimeValidator();
    }

    @Test
    void 終了時刻が開始時刻より後の場合は有効() {
        TimeRange timeRange = new TestTimeRange(LocalTime.of(9, 0), LocalTime.of(10, 0));
        assertTrue(validator.isValid(timeRange, null));
    }

    @Test
    void 終了時刻が開始時刻と同じ場合は無効() {
        TimeRange timeRange = new TestTimeRange(LocalTime.of(9, 0), LocalTime.of(9, 0));
        assertFalse(validator.isValid(timeRange, null));
    }

    @Test
    void 終了時刻が開始時刻より前の場合は無効() {
        TimeRange timeRange = new TestTimeRange(LocalTime.of(10, 0), LocalTime.of(9, 0));
        assertFalse(validator.isValid(timeRange, null));
    }

    @Test
    void nullの場合は有効() {
        assertTrue(validator.isValid(null, null));
    }

    @Test
    void 開始時刻がnullの場合は有効() {
        TimeRange timeRange = new TestTimeRange(null, LocalTime.of(10, 0));
        assertTrue(validator.isValid(timeRange, null));
    }

    @Test
    void 終了時刻がnullの場合は有効() {
        TimeRange timeRange = new TestTimeRange(LocalTime.of(9, 0), null);
        assertTrue(validator.isValid(timeRange, null));
    }

    private static class TestTimeRange implements TimeRange {
        private final LocalTime startTime;
        private final LocalTime endTime;

        public TestTimeRange(LocalTime startTime, LocalTime endTime) {
            this.startTime = startTime;
            this.endTime = endTime;
        }

        @Override
        public LocalTime getStartTime() {
            return startTime;
        }

        @Override
        public LocalTime getEndTime() {
            return endTime;
        }
    }
}
