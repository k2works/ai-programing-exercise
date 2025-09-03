package mrs.infrastructure.in.web;

import mrs.application.domain.model.UserRegistration;
import mrs.application.dto.UserRegistrationRequest;
import mrs.application.dto.UserRegistrationResponse;
import mrs.application.service.UserRegistrationService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import jakarta.validation.Valid;
import java.util.List;
import java.util.Map;
import java.util.Optional;

/**
 * ユーザー自己登録 REST API
 */
@RestController
@RequestMapping("/api/user-registration")
@Validated
public class UserRegistrationController {
    
    @Autowired
    private UserRegistrationService userRegistrationService;
    
    /**
     * ユーザー自己登録申請
     */
    @PostMapping("/register")
    public ResponseEntity<UserRegistrationResponse> registerUser(@Valid @RequestBody UserRegistrationRequest request) {
        UserRegistrationResponse response = userRegistrationService.registerUser(request);
        
        // エラーまたはユーザー存在の場合は400 Bad Request
        if ("ERROR".equals(response.getStatus()) || "REJECTED".equals(response.getStatus())) {
            return ResponseEntity.badRequest().body(response);
        }
        
        // 正常登録の場合は201 Created
        return ResponseEntity.status(HttpStatus.CREATED).body(response);
    }
    
    /**
     * 登録申請一覧取得（管理者用）
     */
    @GetMapping("/list")
    @PreAuthorize("hasRole('ADMIN')")
    public ResponseEntity<List<UserRegistration>> getAllRegistrations() {
        List<UserRegistration> registrations = userRegistrationService.getAllRegistrations();
        return ResponseEntity.ok(registrations);
    }
    
    /**
     * 承認待ち登録申請一覧取得（管理者用）
     */
    @GetMapping("/pending")
    @PreAuthorize("hasRole('ADMIN')")
    public ResponseEntity<List<UserRegistration>> getPendingRegistrations() {
        List<UserRegistration> registrations = userRegistrationService.getPendingRegistrations();
        return ResponseEntity.ok(registrations);
    }
    
    /**
     * 登録申請詳細取得（管理者用）
     */
    @GetMapping("/{registrationId}")
    @PreAuthorize("hasRole('ADMIN')")
    public ResponseEntity<UserRegistration> getRegistrationById(@PathVariable Long registrationId) {
        Optional<UserRegistration> registration = userRegistrationService.getRegistrationById(registrationId);
        return registration
            .map(ResponseEntity::ok)
            .orElse(ResponseEntity.notFound().build());
    }
    
    /**
     * 登録申請承認（管理者用）
     */
    @PostMapping("/{registrationId}/approve")
    @PreAuthorize("hasRole('ADMIN')")
    public ResponseEntity<Map<String, String>> approveRegistration(
            @PathVariable Long registrationId, 
            @RequestBody Map<String, String> request) {
        
        String approvedBy = request.get("approvedBy");
        if (approvedBy == null || approvedBy.trim().isEmpty()) {
            return ResponseEntity.badRequest().body(Map.of("error", "承認者の指定が必要です"));
        }
        
        return handleRegistrationOperation(
            () -> userRegistrationService.approveRegistration(registrationId, approvedBy),
            "登録申請を承認しました",
            "承認処理中にエラーが発生しました"
        );
    }
    
    private ResponseEntity<Map<String, String>> handleRegistrationOperation(
            Runnable operation, String successMessage, String errorMessage) {
        try {
            operation.run();
            return ResponseEntity.ok(Map.of("message", successMessage));
        } catch (IllegalArgumentException e) {
            return ResponseEntity.notFound().build();
        } catch (IllegalStateException e) {
            return ResponseEntity.badRequest().body(Map.of("error", e.getMessage()));
        } catch (Exception e) {
            return ResponseEntity.internalServerError().body(Map.of("error", errorMessage));
        }
    }
    
    /**
     * 登録申請拒否（管理者用）
     */
    @PostMapping("/{registrationId}/reject")
    @PreAuthorize("hasRole('ADMIN')")
    public ResponseEntity<Map<String, String>> rejectRegistration(
            @PathVariable Long registrationId, 
            @RequestBody Map<String, String> request) {
        
        String rejectedBy = request.get("rejectedBy");
        String rejectionReason = request.get("rejectionReason");
        
        if (rejectedBy == null || rejectedBy.trim().isEmpty()) {
            return ResponseEntity.badRequest().body(Map.of("error", "拒否者の指定が必要です"));
        }
        if (rejectionReason == null || rejectionReason.trim().isEmpty()) {
            return ResponseEntity.badRequest().body(Map.of("error", "拒否理由の指定が必要です"));
        }
        
        return handleRegistrationOperation(
            () -> userRegistrationService.rejectRegistration(registrationId, rejectedBy, rejectionReason),
            "登録申請を拒否しました",
            "拒否処理中にエラーが発生しました"
        );
    }
    
    /**
     * 承認待ち件数取得（管理者用）
     */
    @GetMapping("/pending/count")
    @PreAuthorize("hasRole('ADMIN')")
    public ResponseEntity<Map<String, Integer>> getPendingRegistrationCount() {
        int count = userRegistrationService.getPendingRegistrationCount();
        return ResponseEntity.ok(Map.of("count", count));
    }
    
    /**
     * 登録申請削除（管理用）
     */
    @DeleteMapping("/{registrationId}")
    @PreAuthorize("hasRole('ADMIN')")
    public ResponseEntity<Map<String, String>> deleteRegistration(@PathVariable Long registrationId) {
        return handleRegistrationOperation(
            () -> userRegistrationService.deleteRegistration(registrationId),
            "登録申請を削除しました",
            "削除処理中にエラーが発生しました"
        );
    }
}