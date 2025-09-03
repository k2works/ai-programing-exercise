package mrs.application.domain.model;

import mrs.application.domain.model.auth.User;
import java.time.LocalDateTime;

/**
 * ユーザー登録申請エンティティ
 */
public class UserRegistration {
    
    public enum Status {
        PENDING_APPROVAL("承認待ち"),
        APPROVED("承認済み"),
        REJECTED("拒否");
        
        private final String displayName;
        
        Status(String displayName) {
            this.displayName = displayName;
        }
        
        public String getDisplayName() {
            return displayName;
        }
    }
    
    private Long registrationId;
    private String userId;
    private String name;
    private String email;
    private String passwordHash;
    private Status status;
    private LocalDateTime registeredAt;
    private LocalDateTime approvedAt;
    private String approvedBy;
    private LocalDateTime rejectedAt;
    private String rejectedBy;
    private String rejectionReason;
    private LocalDateTime createdAt;
    private LocalDateTime updatedAt;
    
    // デフォルトコンストラクタ
    public UserRegistration() { }
    
    // 新規登録用コンストラクタ
    public UserRegistration(String userId, String name, String email, String passwordHash) {
        this.userId = userId;
        this.name = name;
        this.email = email;
        this.passwordHash = passwordHash;
        this.status = Status.PENDING_APPROVAL;
        this.registeredAt = LocalDateTime.now();
        this.createdAt = LocalDateTime.now();
        this.updatedAt = LocalDateTime.now();
    }
    
    // 承認メソッド
    public void approve(String approvedBy) {
        this.status = Status.APPROVED;
        this.approvedAt = LocalDateTime.now();
        this.approvedBy = approvedBy;
        this.updatedAt = LocalDateTime.now();
    }
    
    // 拒否メソッド
    public void reject(String rejectedBy, String rejectionReason) {
        this.status = Status.REJECTED;
        this.rejectedAt = LocalDateTime.now();
        this.rejectedBy = rejectedBy;
        this.rejectionReason = rejectionReason;
        this.updatedAt = LocalDateTime.now();
    }
    
    // 承認済み判定
    public boolean isApproved() {
        return status == Status.APPROVED;
    }
    
    // 承認待ち判定
    public boolean isPendingApproval() {
        return status == Status.PENDING_APPROVAL;
    }
    
    // 拒否判定
    public boolean isRejected() {
        return status == Status.REJECTED;
    }
    
    // Userエンティティへの変換
    public User toUser() {
        if (!isApproved()) {
            throw new IllegalStateException("承認されていないユーザー登録申請からUserエンティティは作成できません");
        }
        User user = new User();
        user.setUserId(userId);
        user.setName(name);
        user.setPasswordHash(passwordHash);
        user.setRole("USER");
        return user;
    }
    
    // Getters and Setters
    public Long getRegistrationId() {
        return registrationId;
    }
    
    public void setRegistrationId(Long registrationId) {
        this.registrationId = registrationId;
    }
    
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
    
    public String getPasswordHash() {
        return passwordHash;
    }
    
    public void setPasswordHash(String passwordHash) {
        this.passwordHash = passwordHash;
    }
    
    public Status getStatus() {
        return status;
    }
    
    public void setStatus(Status status) {
        this.status = status;
    }
    
    public LocalDateTime getRegisteredAt() {
        return registeredAt;
    }
    
    public void setRegisteredAt(LocalDateTime registeredAt) {
        this.registeredAt = registeredAt;
    }
    
    public LocalDateTime getApprovedAt() {
        return approvedAt;
    }
    
    public void setApprovedAt(LocalDateTime approvedAt) {
        this.approvedAt = approvedAt;
    }
    
    public String getApprovedBy() {
        return approvedBy;
    }
    
    public void setApprovedBy(String approvedBy) {
        this.approvedBy = approvedBy;
    }
    
    public LocalDateTime getRejectedAt() {
        return rejectedAt;
    }
    
    public void setRejectedAt(LocalDateTime rejectedAt) {
        this.rejectedAt = rejectedAt;
    }
    
    public String getRejectedBy() {
        return rejectedBy;
    }
    
    public void setRejectedBy(String rejectedBy) {
        this.rejectedBy = rejectedBy;
    }
    
    public String getRejectionReason() {
        return rejectionReason;
    }
    
    public void setRejectionReason(String rejectionReason) {
        this.rejectionReason = rejectionReason;
    }
    
    public LocalDateTime getCreatedAt() {
        return createdAt;
    }
    
    public void setCreatedAt(LocalDateTime createdAt) {
        this.createdAt = createdAt;
    }
    
    public LocalDateTime getUpdatedAt() {
        return updatedAt;
    }
    
    public void setUpdatedAt(LocalDateTime updatedAt) {
        this.updatedAt = updatedAt;
    }
    
    @Override
    public String toString() {
        return "UserRegistration{" +
                "registrationId=" + registrationId +
                ", userId='" + userId + '\'' +
                ", name='" + name + '\'' +
                ", email='" + email + '\'' +
                ", status=" + status +
                ", registeredAt=" + registeredAt +
                ", approvedAt=" + approvedAt +
                ", approvedBy='" + approvedBy + '\'' +
                '}';
    }
}