package mrs.common.validation;

import jakarta.validation.Constraint;
import jakarta.validation.Payload;
import java.lang.annotation.*;

@Target({ElementType.FIELD, ElementType.PARAMETER})
@Retention(RetentionPolicy.RUNTIME)
@Constraint(validatedBy = ThirtyMinutesUnitValidator.class)
@Documented
public @interface ThirtyMinutesUnit {
    String message() default "Time must be in 30-minute units";
    Class<?>[] groups() default {};
    Class<? extends Payload>[] payload() default {};
}
