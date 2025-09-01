package mrs.common.exception;

/**
 * 認証失敗時の例外
 * ユーザー認証やトークン検証の失敗時に発生する
 */
public class AuthenticationException extends RuntimeException {
    public AuthenticationException(String message) {
        super(message);
    }

    public AuthenticationException(String message, Throwable cause) {
        super(message, cause);
    }
}