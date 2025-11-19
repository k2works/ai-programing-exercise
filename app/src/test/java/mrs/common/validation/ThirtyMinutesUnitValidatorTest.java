package mrs.common.validation;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.time.LocalTime;

import static org.junit.jupiter.api.Assertions.*;

class ThirtyMinutesUnitValidatorTest {
    private ThirtyMinutesUnitValidator validator;

    @BeforeEach
    void setUp() {
        validator = new ThirtyMinutesUnitValidator();
    }

    @Test
    void 時刻が30分単位の場合は有効() {
        assertTrue(validator.isValid(LocalTime.of(9, 0), null));
        assertTrue(validator.isValid(LocalTime.of(9, 30), null));
        assertTrue(validator.isValid(LocalTime.of(10, 0), null));
    }

    @Test
    void 時刻が30分単位でない場合は無効() {
        assertFalse(validator.isValid(LocalTime.of(9, 15), null));
        assertFalse(validator.isValid(LocalTime.of(9, 45), null));
        assertFalse(validator.isValid(LocalTime.of(10, 1), null));
    }

    @Test
    void 秒が0でない場合は無効() {
        assertFalse(validator.isValid(LocalTime.of(9, 0, 1), null));
    }

    @Test
    void nullの場合は有効() {
        assertTrue(validator.isValid(null, null));
    }
}
