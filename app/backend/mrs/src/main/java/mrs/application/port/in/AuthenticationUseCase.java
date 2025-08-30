package mrs.application.port.in;

import mrs.application.dto.LoginRequest;
import mrs.application.dto.LoginResponse;
import mrs.application.exception.AuthenticationException;

/**
 * 認証機能のユースケースインターフェース
 * Web層からのアクセスポイントとして認証関連の操作を定義
 */
public interface AuthenticationUseCase {

    /**
     * ユーザー認証を実行
     * 
     * @param loginRequest ログイン情報（ユーザーID、パスワード）
     * @return ログインレスポンス（JWTトークン情報）
     * @throws AuthenticationException 認証失敗時
     */
    LoginResponse authenticate(LoginRequest loginRequest);

    /**
     * JWTトークンのリフレッシュを実行
     * 
     * @param token 既存のJWTトークン
     * @return 新しいログインレスポンス（リフレッシュされたJWTトークン）
     * @throws AuthenticationException トークン不正時
     */
    LoginResponse refreshToken(String token);
}