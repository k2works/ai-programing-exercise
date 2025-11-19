package mrs.infrastructure.in.web.form;

import jakarta.validation.ConstraintViolation;
import jakarta.validation.Validation;
import jakarta.validation.Validator;
import jakarta.validation.ValidatorFactory;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.time.LocalTime;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.*;

class ReservationFormTest {
    private Validator validator;

    @BeforeEach
    void setUp() {
        ValidatorFactory factory = Validation.buildDefaultValidatorFactory();
        validator = factory.getValidator();
    }

    @Test
    void 有効なフォームはバリデーションエラーなし() {
        ReservationForm form = new ReservationForm(LocalTime.of(9, 0), LocalTime.of(10, 0));
        Set<ConstraintViolation<ReservationForm>> violations = validator.validate(form);
        assertTrue(violations.isEmpty());
    }

    @Test
    void 開始時刻がnullの場合はエラー() {
        ReservationForm form = new ReservationForm(null, LocalTime.of(10, 0));
        Set<ConstraintViolation<ReservationForm>> violations = validator.validate(form);
        assertEquals(1, violations.size());
        assertTrue(violations.stream()
            .anyMatch(v -> v.getMessage().contains("Start time is required")));
    }

    @Test
    void 終了時刻がnullの場合はエラー() {
        ReservationForm form = new ReservationForm(LocalTime.of(9, 0), null);
        Set<ConstraintViolation<ReservationForm>> violations = validator.validate(form);
        assertEquals(1, violations.size());
        assertTrue(violations.stream()
            .anyMatch(v -> v.getMessage().contains("End time is required")));
    }

    @Test
    void 開始時刻が30分単位でない場合はエラー() {
        ReservationForm form = new ReservationForm(LocalTime.of(9, 15), LocalTime.of(10, 0));
        Set<ConstraintViolation<ReservationForm>> violations = validator.validate(form);
        assertFalse(violations.isEmpty());
        assertTrue(violations.stream()
            .anyMatch(v -> v.getMessage().contains("30-minute units")));
    }

    @Test
    void 終了時刻が30分単位でない場合はエラー() {
        ReservationForm form = new ReservationForm(LocalTime.of(9, 0), LocalTime.of(10, 15));
        Set<ConstraintViolation<ReservationForm>> violations = validator.validate(form);
        assertFalse(violations.isEmpty());
        assertTrue(violations.stream()
            .anyMatch(v -> v.getMessage().contains("30-minute units")));
    }

    @Test
    void 終了時刻が開始時刻より前の場合はエラー() {
        ReservationForm form = new ReservationForm(LocalTime.of(10, 0), LocalTime.of(9, 0));
        Set<ConstraintViolation<ReservationForm>> violations = validator.validate(form);
        assertFalse(violations.isEmpty());
        assertTrue(violations.stream()
            .anyMatch(v -> v.getMessage().contains("End time must be after start time")));
    }

    @Test
    void 終了時刻が開始時刻と同じ場合はエラー() {
        ReservationForm form = new ReservationForm(LocalTime.of(9, 0), LocalTime.of(9, 0));
        Set<ConstraintViolation<ReservationForm>> violations = validator.validate(form);
        assertFalse(violations.isEmpty());
        assertTrue(violations.stream()
            .anyMatch(v -> v.getMessage().contains("End time must be after start time")));
    }
}
