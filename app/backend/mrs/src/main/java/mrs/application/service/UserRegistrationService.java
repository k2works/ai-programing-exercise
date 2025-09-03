package mrs.application.service;

import mrs.application.domain.model.auth.User;
import mrs.application.domain.model.UserRegistration;
import mrs.application.dto.UserRegistrationRequest;
import mrs.application.dto.UserRegistrationResponse;
import mrs.infrastructure.out.persistence.UserMapper;
import mrs.infrastructure.out.persistence.UserRegistrationMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Optional;

/**
 * ユーザー自己登録サービス
 */
@Service
@Transactional
public class UserRegistrationService {
    
    @Autowired
    private UserRegistrationMapper userRegistrationMapper;
    
    @Autowired
    private UserMapper userMapper;
    
    @Autowired
    private PasswordEncoder passwordEncoder;
    
    /**
     * ユーザー自己登録申請
     */
    public UserRegistrationResponse registerUser(UserRegistrationRequest request) {
        try {
            // ユーザーID重複チェック
            if (userRegistrationMapper.existsUserIdInRegistrationOrUser(request.getUserId())) {
                return UserRegistrationResponse.userExists(request.getUserId());
            }
            
            // メールアドレス重複チェック
            if (userRegistrationMapper.existsEmailInRegistration(request.getEmail())) {
                return UserRegistrationResponse.error("指定されたメールアドレスは既に使用されています。");
            }
            
            // パスワードハッシュ化
            String passwordHash = passwordEncoder.encode(request.getPassword());
            
            // 登録申請エンティティ作成
            UserRegistration registration = new UserRegistration(
                request.getUserId(),
                request.getName(),
                request.getEmail(),
                passwordHash
            );
            
            // 登録申請を保存
            userRegistrationMapper.insert(registration);
            
            return UserRegistrationResponse.success(
                registration.getUserId(),
                registration.getName(),
                registration.getEmail(),
                registration.getRegisteredAt()
            );
            
        } catch (Exception e) {
            return UserRegistrationResponse.error("登録処理中にエラーが発生しました: " + e.getMessage());
        }
    }
    
    /**
     * 登録申請一覧取得（管理者用）
     */
    @Transactional(readOnly = true)
    public List<UserRegistration> getAllRegistrations() {
        return userRegistrationMapper.findAll();
    }
    
    /**
     * 承認待ち登録申請一覧取得（管理者用）
     */
    @Transactional(readOnly = true)
    public List<UserRegistration> getPendingRegistrations() {
        return userRegistrationMapper.findPendingRegistrations();
    }
    
    /**
     * 登録申請詳細取得
     */
    @Transactional(readOnly = true)
    public Optional<UserRegistration> getRegistrationById(Long registrationId) {
        return userRegistrationMapper.findByRegistrationId(registrationId);
    }
    
    /**
     * 登録申請承認（管理者用）
     */
    public void approveRegistration(Long registrationId, String approvedBy) {
        Optional<UserRegistration> registrationOpt = userRegistrationMapper.findByRegistrationId(registrationId);
        if (registrationOpt.isEmpty()) {
            throw new IllegalArgumentException("登録申請が見つかりません: " + registrationId);
        }
        
        UserRegistration registration = registrationOpt.get();
        if (!registration.isPendingApproval()) {
            throw new IllegalStateException("承認待ち状態ではない登録申請は承認できません");
        }
        
        // 登録申請を承認状態に更新
        userRegistrationMapper.approve(registrationId, approvedBy);
        
        // 実際のユーザーを作成してusrテーブルに追加
        registration.approve(approvedBy);
        User newUser = registration.toUser();
        userMapper.insert(newUser);
    }
    
    /**
     * 登録申請拒否（管理者用）
     */
    public void rejectRegistration(Long registrationId, String rejectedBy, String rejectionReason) {
        Optional<UserRegistration> registrationOpt = userRegistrationMapper.findByRegistrationId(registrationId);
        if (registrationOpt.isEmpty()) {
            throw new IllegalArgumentException("登録申請が見つかりません: " + registrationId);
        }
        
        UserRegistration registration = registrationOpt.get();
        if (!registration.isPendingApproval()) {
            throw new IllegalStateException("承認待ち状態ではない登録申請は拒否できません");
        }
        
        // 登録申請を拒否状態に更新
        userRegistrationMapper.reject(registrationId, rejectedBy, rejectionReason);
    }
    
    /**
     * 承認待ち件数取得
     */
    @Transactional(readOnly = true)
    public int getPendingRegistrationCount() {
        return userRegistrationMapper.countPendingRegistrations();
    }
    
    /**
     * 登録申請削除（管理用）
     */
    public void deleteRegistration(Long registrationId) {
        if (userRegistrationMapper.findByRegistrationId(registrationId).isEmpty()) {
            throw new IllegalArgumentException("登録申請が見つかりません: " + registrationId);
        }
        userRegistrationMapper.delete(registrationId);
    }
}