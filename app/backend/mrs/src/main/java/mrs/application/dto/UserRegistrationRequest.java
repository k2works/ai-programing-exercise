package mrs.application.dto;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Pattern;
import jakarta.validation.constraints.Size;

/**
 * ユーザー自己登録リクエストDTO
 */
public class UserRegistrationRequest {
    
    @NotBlank(message = "ユーザーIDは必須です")
    @Size(min = 3, max = 50, message = "ユーザーIDは3文字以上50文字以下で入力してください")
    @Pattern(regexp = "^[a-zA-Z0-9_-]+$", message = "ユーザーIDは英数字、アンダースコア、ハイフンのみ使用可能です")
    private String userId;
    
    @NotBlank(message = "氏名は必須です")
    @Size(max = 100, message = "氏名は100文字以下で入力してください")
    private String name;
    
    @NotBlank(message = "パスワードは必須です")
    @Size(min = 6, max = 100, message = "パスワードは6文字以上100文字以下で入力してください")
    @Pattern(regexp = "^(?=.*[a-zA-Z])(?=.*\\d)[a-zA-Z\\d!@#$%^&*()_+\\-=\\[\\]{};':\",./<>?]+$", 
             message = "パスワードは英字と数字を含む必要があります")
    private String password;
    
    @NotBlank(message = "メールアドレスは必須です")
    @Pattern(regexp = "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$", 
             message = "正しいメールアドレス形式で入力してください")
    private String email;
    
    // デフォルトコンストラクタ
    public UserRegistrationRequest() { }
    
    // 全項目コンストラクタ
    public UserRegistrationRequest(String userId, String name, String password, String email) {
        this.userId = userId;
        this.name = name;
        this.password = password;
        this.email = email;
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
    
    public String getPassword() {
        return password;
    }
    
    public void setPassword(String password) {
        this.password = password;
    }
    
    public String getEmail() {
        return email;
    }
    
    public void setEmail(String email) {
        this.email = email;
    }
    
    @Override
    public String toString() {
        return "UserRegistrationRequest{" +
                "userId='" + userId + '\'' +
                ", name='" + name + '\'' +
                ", email='" + email + '\'' +
                ", password='[PROTECTED]'" +
                '}';
    }
}