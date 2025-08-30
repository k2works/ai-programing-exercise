package mrs.application.dto;

/**
 * ログインレスポンスDTO
 * 認証成功時のJWTトークン情報を格納
 */
public class LoginResponse {
    private String accessToken;
    private String tokenType = "Bearer";
    private long expiresIn;

    public LoginResponse() {
    }

    public LoginResponse(String accessToken, long expiresIn) {
        this.accessToken = accessToken;
        this.expiresIn = expiresIn;
    }

    // 利便性のための追加のgetterメソッド（テスト用）
    public String getToken() {
        return accessToken;
    }

    public String getAccessToken() {
        return accessToken;
    }

    public void setAccessToken(String accessToken) {
        this.accessToken = accessToken;
    }

    public String getTokenType() {
        return tokenType;
    }

    public void setTokenType(String tokenType) {
        this.tokenType = tokenType;
    }

    public long getExpiresIn() {
        return expiresIn;
    }

    public void setExpiresIn(long expiresIn) {
        this.expiresIn = expiresIn;
    }
}