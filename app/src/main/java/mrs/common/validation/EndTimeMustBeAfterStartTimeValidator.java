package mrs.common.validation;

import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;

import java.time.LocalTime;

public class EndTimeMustBeAfterStartTimeValidator implements ConstraintValidator<EndTimeMustBeAfterStartTime, TimeRange> {

    @Override
    public boolean isValid(TimeRange value, ConstraintValidatorContext context) {
        if (value == null) {
            return true;
        }
        LocalTime startTime = value.getStartTime();
        LocalTime endTime = value.getEndTime();
        
        if (startTime == null || endTime == null) {
            return true;
        }
        
        return endTime.isAfter(startTime);
    }
}
