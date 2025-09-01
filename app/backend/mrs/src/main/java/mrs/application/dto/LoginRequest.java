package mrs.application.dto;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * ログインリクエストDTO
 * Web層とアプリケーション層を分離するためのデータ転送オブジェクト
 */
public class LoginRequest {
    @JsonProperty("userId")
    private String username;
    private String password;

    public LoginRequest() {
    }

    public LoginRequest(String username, String password) {
        this.username = username;
        this.password = password;
    }

    public String getUsername() {
        return username;
    }

    public void setUsername(String username) {
        this.username = username;
    }

    public String getPassword() {
        return password;
    }

    public void setPassword(String password) {
        this.password = password;
    }
}