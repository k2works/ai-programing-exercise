package mrs.infrastructure.in.web;

import mrs.application.domain.model.UserRegistration;
import mrs.application.service.UserRegistrationService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

import java.util.List;
import java.util.Optional;

/**
 * 管理者画面コントローラー
 */
@Controller
@RequestMapping("/admin")
@PreAuthorize("hasRole('ADMIN')")
public class AdminController {
    
    @Autowired
    private UserRegistrationService userRegistrationService;
    
    /**
     * 管理者ダッシュボード
     */
    @GetMapping("/dashboard")
    public String dashboard(Model model) {
        // 承認待ち件数を取得
        int pendingCount = userRegistrationService.getPendingRegistrationCount();
        model.addAttribute("pendingRegistrationCount", pendingCount);
        
        return "admin/dashboard";
    }
    
    /**
     * ユーザー登録申請管理画面
     */
    @GetMapping("/user-registrations")
    public String userRegistrations(Model model) {
        List<UserRegistration> allRegistrations = userRegistrationService.getAllRegistrations();
        List<UserRegistration> pendingRegistrations = userRegistrationService.getPendingRegistrations();
        
        model.addAttribute("allRegistrations", allRegistrations);
        model.addAttribute("pendingRegistrations", pendingRegistrations);
        model.addAttribute("pendingCount", pendingRegistrations.size());
        
        return "admin/user-registrations";
    }
    
    /**
     * 登録申請詳細画面
     */
    @GetMapping("/user-registrations/{registrationId}")
    public String registrationDetail(@PathVariable Long registrationId, Model model) {
        Optional<UserRegistration> registration = userRegistrationService.getRegistrationById(registrationId);
        
        if (registration.isEmpty()) {
            return "redirect:/admin/user-registrations?error=notfound";
        }
        
        model.addAttribute("registration", registration.get());
        return "admin/registration-detail";
    }
    
    /**
     * 登録申請承認処理
     */
    @PostMapping("/user-registrations/{registrationId}/approve")
    public String approveRegistration(
            @PathVariable Long registrationId,
            @RequestParam String approvedBy,
            RedirectAttributes redirectAttributes) {
        
        try {
            userRegistrationService.approveRegistration(registrationId, approvedBy);
            redirectAttributes.addFlashAttribute("successMessage", "登録申請を承認しました。");
        } catch (Exception e) {
            redirectAttributes.addFlashAttribute("errorMessage", "承認処理中にエラーが発生しました: " + e.getMessage());
        }
        
        return "redirect:/admin/user-registrations";
    }
    
    /**
     * 登録申請拒否処理
     */
    @PostMapping("/user-registrations/{registrationId}/reject")
    public String rejectRegistration(
            @PathVariable Long registrationId,
            @RequestParam String rejectedBy,
            @RequestParam String rejectionReason,
            RedirectAttributes redirectAttributes) {
        
        try {
            if (rejectionReason == null || rejectionReason.trim().isEmpty()) {
                redirectAttributes.addFlashAttribute("errorMessage", "拒否理由を入力してください。");
                return "redirect:/admin/user-registrations/" + registrationId;
            }
            
            userRegistrationService.rejectRegistration(registrationId, rejectedBy, rejectionReason);
            redirectAttributes.addFlashAttribute("successMessage", "登録申請を拒否しました。");
        } catch (Exception e) {
            redirectAttributes.addFlashAttribute("errorMessage", "拒否処理中にエラーが発生しました: " + e.getMessage());
        }
        
        return "redirect:/admin/user-registrations";
    }
    
    /**
     * 登録申請削除処理
     */
    @PostMapping("/user-registrations/{registrationId}/delete")
    public String deleteRegistration(
            @PathVariable Long registrationId,
            RedirectAttributes redirectAttributes) {
        
        try {
            userRegistrationService.deleteRegistration(registrationId);
            redirectAttributes.addFlashAttribute("successMessage", "登録申請を削除しました。");
        } catch (Exception e) {
            redirectAttributes.addFlashAttribute("errorMessage", "削除処理中にエラーが発生しました: " + e.getMessage());
        }
        
        return "redirect:/admin/user-registrations";
    }
}