package mrs.common.exception;

/**
 * 予約が見つからない場合の例外
 * 指定されたIDの予約が存在しない場合に発生する
 */
public class ReservationNotFoundException extends RuntimeException {
    public ReservationNotFoundException(String message) {
        super(message);
    }

    public ReservationNotFoundException(String message, Throwable cause) {
        super(message, cause);
    }
}