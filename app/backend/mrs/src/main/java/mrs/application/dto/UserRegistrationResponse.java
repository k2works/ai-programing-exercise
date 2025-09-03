package mrs.application.dto;

import java.time.LocalDateTime;

/**
 * ユーザー自己登録レスポンスDTO
 */
public class UserRegistrationResponse {
    
    private String userId;
    private String name;
    private String email;
    private String status; // PENDING_APPROVAL, APPROVED, REJECTED
    private LocalDateTime registeredAt;
    private String message;
    
    // デフォルトコンストラクタ
    public UserRegistrationResponse() { }
    
    // 成功レスポンス用コンストラクタ
    public UserRegistrationResponse(String userId, String name, String email, String status, LocalDateTime registeredAt, String message) {
        this.userId = userId;
        this.name = name;
        this.email = email;
        this.status = status;
        this.registeredAt = registeredAt;
        this.message = message;
    }
    
    // ファクトリーメソッド - 登録成功
    public static UserRegistrationResponse success(String userId, String name, String email, LocalDateTime registeredAt) {
        return new UserRegistrationResponse(
            userId, 
            name, 
            email, 
            "PENDING_APPROVAL", 
            registeredAt,
            "ユーザー登録が完了しました。管理者の承認をお待ちください。"
        );
    }
    
    // ファクトリーメソッド - 登録失敗（重複ユーザー）
    public static UserRegistrationResponse userExists(String userId) {
        return new UserRegistrationResponse(
            userId, 
            null, 
            null, 
            "REJECTED", 
            LocalDateTime.now(),
            "指定されたユーザーIDは既に使用されています。"
        );
    }
    
    // ファクトリーメソッド - 登録失敗（エラー）
    public static UserRegistrationResponse error(String message) {
        return new UserRegistrationResponse(
            null, 
            null, 
            null, 
            "ERROR", 
            LocalDateTime.now(),
            message
        );
    }
    
    // Getters and Setters
    public String getUserId() {
        return userId;
    }
    
    public void setUserId(String userId) {
        this.userId = userId;
    }
    
    public String getName() {
        return name;
    }
    
    public void setName(String name) {
        this.name = name;
    }
    
    public String getEmail() {
        return email;
    }
    
    public void setEmail(String email) {
        this.email = email;
    }
    
    public String getStatus() {
        return status;
    }
    
    public void setStatus(String status) {
        this.status = status;
    }
    
    public LocalDateTime getRegisteredAt() {
        return registeredAt;
    }
    
    public void setRegisteredAt(LocalDateTime registeredAt) {
        this.registeredAt = registeredAt;
    }
    
    public String getMessage() {
        return message;
    }
    
    public void setMessage(String message) {
        this.message = message;
    }
    
    @Override
    public String toString() {
        return "UserRegistrationResponse{" +
                "userId='" + userId + '\'' +
                ", name='" + name + '\'' +
                ", email='" + email + '\'' +
                ", status='" + status + '\'' +
                ", registeredAt=" + registeredAt +
                ", message='" + message + '\'' +
                '}';
    }
}