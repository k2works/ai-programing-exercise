package mrs.infrastructure.in.web;

import mrs.application.service.reservation.AlreadyReservedException;
import mrs.application.service.reservation.UnavailableReservationException;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;

@ControllerAdvice
public class GlobalExceptionHandler {

    @ExceptionHandler(AlreadyReservedException.class)
    public String handleAlreadyReservedException(AlreadyReservedException e, Model model) {
        model.addAttribute("error", "指定の時間帯は既に予約されています。");
        return "error";
    }

    @ExceptionHandler(UnavailableReservationException.class)
    public String handleUnavailableReservationException(UnavailableReservationException e, Model model) {
        model.addAttribute("error", "指定の会議室は予約できません。");
        return "error";
    }

    @ExceptionHandler(SecurityException.class)
    public String handleSecurityException(SecurityException e, Model model) {
        model.addAttribute("error", "この操作を実行する権限がありません。");
        return "error";
    }

    @ExceptionHandler(Exception.class)
    public String handleException(Exception e, Model model) {
        model.addAttribute("error", "予期しないエラーが発生しました。");
        return "error";
    }
}
