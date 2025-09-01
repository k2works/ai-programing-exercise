package mrs.application.validation;

import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;
import mrs.application.dto.ReservationRequest;

public class TimeRangeValidator implements ConstraintValidator<ValidTimeRange, ReservationRequest> {
    
    @Override
    public void initialize(ValidTimeRange constraintAnnotation) {
        // 初期化処理なし
    }
    
    @Override
    public boolean isValid(ReservationRequest request, ConstraintValidatorContext context) {
        if (request == null || request.getStartTime() == null || request.getEndTime() == null) {
            return true; // null チェックは他のバリデータに任せる
        }
        
        // 開始時間は終了時間より前である必要がある
        boolean isValid = request.getStartTime().isBefore(request.getEndTime());
        
        if (!isValid) {
            context.disableDefaultConstraintViolation();
            context.buildConstraintViolationWithTemplate("終了時間は開始時間より後である必要があります")
                   .addPropertyNode("endTime")
                   .addConstraintViolation();
        }
        
        return isValid;
    }
}