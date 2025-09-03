package mrs.infrastructure.out.persistence;

import mrs.application.domain.model.UserRegistration;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;
import java.util.Optional;

/**
 * ユーザー登録申請 MyBatis Mapper
 */
@Mapper
public interface UserRegistrationMapper {
    
    /**
     * ユーザー登録申請を新規作成
     */
    void insert(UserRegistration userRegistration);
    
    /**
     * 登録申請IDによる検索
     */
    Optional<UserRegistration> findByRegistrationId(@Param("registrationId") Long registrationId);
    
    /**
     * ユーザーIDによる検索
     */
    Optional<UserRegistration> findByUserId(@Param("userId") String userId);
    
    /**
     * メールアドレスによる検索
     */
    Optional<UserRegistration> findByEmail(@Param("email") String email);
    
    /**
     * ステータス別検索
     */
    List<UserRegistration> findByStatus(@Param("status") UserRegistration.Status status);
    
    /**
     * 承認待ちの登録申請一覧取得（管理者用）
     */
    List<UserRegistration> findPendingRegistrations();
    
    /**
     * 全登録申請一覧取得（管理者用）
     */
    List<UserRegistration> findAll();
    
    /**
     * 登録申請の更新
     */
    void update(UserRegistration userRegistration);
    
    /**
     * 承認処理
     */
    void approve(@Param("registrationId") Long registrationId, 
                @Param("approvedBy") String approvedBy);
    
    /**
     * 拒否処理
     */
    void reject(@Param("registrationId") Long registrationId, 
               @Param("rejectedBy") String rejectedBy,
               @Param("rejectionReason") String rejectionReason);
    
    /**
     * 登録申請削除（管理用）
     */
    void delete(@Param("registrationId") Long registrationId);
    
    /**
     * ユーザーID重複チェック（user_registrationとusrテーブル両方）
     */
    boolean existsUserIdInRegistrationOrUser(@Param("userId") String userId);
    
    /**
     * メールアドレス重複チェック
     */
    boolean existsEmailInRegistration(@Param("email") String email);
    
    /**
     * 承認待ち件数取得
     */
    int countPendingRegistrations();
}