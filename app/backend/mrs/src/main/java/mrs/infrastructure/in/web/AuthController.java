package mrs.infrastructure.in.web;

import java.util.Map;
import mrs.security.JwtService;
import mrs.infrastructure.out.db.UserMapper;
import mrs.application.domain.model.User;
import org.springframework.http.ResponseEntity;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/auth")
public class AuthController {
    private final JwtService jwtService;
    private final BCryptPasswordEncoder passwordEncoder;
    private final UserMapper userMapper;

    public AuthController(JwtService jwtService, BCryptPasswordEncoder passwordEncoder, UserMapper userMapper) {
        this.jwtService = jwtService;
        this.passwordEncoder = passwordEncoder;
        this.userMapper = userMapper;
    }

    @PostMapping("/login")
    public ResponseEntity<?> login(@RequestBody Map<String, String> body) {
        String userId = body.getOrDefault("username", "");
        String rawPassword = body.getOrDefault("password", "");

        if (userId.isBlank() || rawPassword.isBlank()) {
            return ResponseEntity.badRequest().body(Map.of("message", "username and password are required"));
        }

        User user = userMapper.findById(userId);
        if (user != null && passwordEncoder.matches(rawPassword, user.getPasswordHash())) {
            String token = jwtService.createAccessToken(user.getUserId(), Map.of("roles", user.getRole()));
            return ResponseEntity.ok(Map.of("accessToken", token));
        }
        return ResponseEntity.status(401).body(Map.of("message", "invalid credentials"));
    }

    @PostMapping("/refresh")
    public ResponseEntity<?> refresh(@RequestHeader(name = "Authorization", required = false) String authHeader) {
        if (authHeader == null || !authHeader.startsWith("Bearer ")) {
            return ResponseEntity.status(401).body(Map.of("message", "missing bearer token"));
        }
        String token = authHeader.substring(7);
        try {
            var claims = jwtService.parseAndValidate(token);
            String userId = claims.getSubject();
            Object roles = claims.get("roles");
            String newToken = jwtService.createAccessToken(userId, Map.of("roles", roles));
            return ResponseEntity.ok(Map.of("accessToken", newToken));
        } catch (Exception e) {
            return ResponseEntity.status(401).body(Map.of("message", "invalid token"));
        }
    }
}
