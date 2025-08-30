package mrs.application.exception;

/**
 * 予約不可時の例外
 * 指定された時間帯が予約できない状況で発生する
 */
public class UnavailableReservationException extends RuntimeException {
    public UnavailableReservationException(String message) {
        super(message);
    }

    public UnavailableReservationException(String message, Throwable cause) {
        super(message, cause);
    }
}