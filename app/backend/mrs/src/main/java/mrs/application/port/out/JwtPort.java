package mrs.application.port.out;

import java.util.Map;

/**
 * JWT関連の外部サービスとのインターフェース
 * Application層とInfrastructure層を分離するためのポート
 */
public interface JwtPort {
    
    /**
     * アクセストークンを作成
     * 
     * @param subject トークンの主体（通常はユーザーID）
     * @param claims トークンに含める追加クレーム
     * @return JWT文字列
     */
    String createAccessToken(String subject, Map<String, Object> claims);

    /**
     * JWTトークンをパースして検証
     * 
     * @param token JWTトークン文字列
     * @return パースされたクレーム
     * @throws RuntimeException トークンが無効または期限切れの場合
     */
    Map<String, Object> parseAndValidate(String token);

    /**
     * アクセストークンの有効期限（秒）を取得
     * 
     * @return 有効期限秒数
     */
    long getExpirationTime();
}