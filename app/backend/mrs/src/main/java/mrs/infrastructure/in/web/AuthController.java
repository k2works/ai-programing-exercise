package mrs.infrastructure.in.web;

import java.util.Map;
import mrs.security.JwtService;
import org.springframework.http.ResponseEntity;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/auth")
public class AuthController {
    private final JwtService jwtService;
    private final BCryptPasswordEncoder passwordEncoder = new BCryptPasswordEncoder();

    public AuthController(JwtService jwtService) {
        this.jwtService = jwtService;
    }

    @PostMapping("/login")
    public ResponseEntity<?> login(@RequestBody Map<String, String> body) {
        String userId = body.getOrDefault("username", "");
        String password = body.getOrDefault("password", "");
        // TODO: Replace with UserMapper.findById and BCrypt verification
        if ("user1".equals(userId) && passwordEncoder.matches(password, passwordEncoder.encode("demo"))) {
            String token = jwtService.createAccessToken(userId, Map.of("roles", "USER"));
            return ResponseEntity.ok(Map.of("accessToken", token));
        }
        return ResponseEntity.status(401).body(Map.of("message", "invalid credentials"));
    }
}
