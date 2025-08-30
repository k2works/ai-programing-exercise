package mrs.application.exception;

/**
 * 既に予約済みの場合の例外
 * 同一時間帯に重複予約が発生する場合に発生する
 */
public class AlreadyReservedException extends RuntimeException {
    public AlreadyReservedException(String message) {
        super(message);
    }

    public AlreadyReservedException(String message, Throwable cause) {
        super(message, cause);
    }
}