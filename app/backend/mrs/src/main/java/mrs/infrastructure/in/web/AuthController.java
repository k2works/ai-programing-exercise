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

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.ExampleObject;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;
import io.swagger.v3.oas.annotations.tags.Tag;

@RestController
@RequestMapping("/api/auth")
@Tag(name = "Authentication", description = "ユーザー認証関連API")
public class AuthController {
    private static final String KEY_MESSAGE = "message";
    private static final String MEDIA_TYPE_JSON = "application/json";
    private final JwtService jwtService;
    private final BCryptPasswordEncoder passwordEncoder;
    private final UserMapper userMapper;

    public AuthController(JwtService jwtService, BCryptPasswordEncoder passwordEncoder, UserMapper userMapper) {
        this.jwtService = jwtService;
        this.passwordEncoder = passwordEncoder;
        this.userMapper = userMapper;
    }

    @PostMapping("/login")
    @Operation(
        summary = "ユーザー認証",
        description = "ユーザーID・パスワードによる認証を行い、JWTアクセストークンを発行する"
    )
    @ApiResponses({
        @ApiResponse(
            responseCode = "200",
            description = "認証成功",
            content = @Content(
                mediaType = MEDIA_TYPE_JSON,
                examples = @ExampleObject(value = "{\"accessToken\": \"eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9...\"}")
            )
        ),
        @ApiResponse(
            responseCode = "400",
            description = "リクエスト不正（必須項目不足）",
            content = @Content(
                mediaType = MEDIA_TYPE_JSON,
                examples = @ExampleObject(value = "{\"message\": \"username and password are required\"}")
            )
        ),
        @ApiResponse(
            responseCode = "401",
            description = "認証失敗",
            content = @Content(
                mediaType = MEDIA_TYPE_JSON,
                examples = @ExampleObject(value = "{\"message\": \"invalid credentials\"}")
            )
        )
    })
    public ResponseEntity<?> login(
        @Parameter(
            description = "認証情報",
            content = @Content(
                examples = @ExampleObject(value = "{\"username\": \"user1\", \"password\": \"demo\"}")
            )
        )
        @RequestBody Map<String, String> body) {
        String userId = body.getOrDefault("username", "");
        String rawPassword = body.getOrDefault("password", "");

        if (userId.isBlank() || rawPassword.isBlank()) {
            return ResponseEntity.badRequest().body(Map.of(KEY_MESSAGE, "username and password are required"));
        }

        User user = userMapper.findById(userId);
        if (user != null && passwordEncoder.matches(rawPassword, user.getPasswordHash())) {
            String token = jwtService.createAccessToken(user.getUserId(), Map.of("roles", user.getRole()));
            return ResponseEntity.ok(Map.of("accessToken", token));
        }
        return ResponseEntity.status(401).body(Map.of(KEY_MESSAGE, "invalid credentials"));
    }

    @PostMapping("/refresh")
    @Operation(
        summary = "トークンリフレッシュ",
        description = "有効なJWTトークンを新しいトークンに更新する"
    )
    @SecurityRequirement(name = "bearerAuth")
    @ApiResponses({
        @ApiResponse(
            responseCode = "200",
            description = "リフレッシュ成功",
            content = @Content(
                mediaType = MEDIA_TYPE_JSON,
                examples = @ExampleObject(value = "{\"accessToken\": \"eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9...\"}")
            )
        ),
        @ApiResponse(
            responseCode = "401",
            description = "トークン不正または期限切れ",
            content = @Content(
                mediaType = MEDIA_TYPE_JSON,
                examples = @ExampleObject(value = "{\"message\": \"missing bearer token\"}")
            )
        )
    })
    public ResponseEntity<?> refresh(
        @Parameter(description = "Authorization: Bearer {JWT_TOKEN}")
        @RequestHeader(name = "Authorization", required = false) String authHeader) {
        if (authHeader == null || !authHeader.startsWith("Bearer ")) {
            return ResponseEntity.status(401).body(Map.of(KEY_MESSAGE, "missing bearer token"));
        }
        String token = authHeader.substring(7);
        try {
            var claims = jwtService.parseAndValidate(token);
            String userId = claims.getSubject();
            Object roles = claims.get("roles");
            String newToken = jwtService.createAccessToken(userId, Map.of("roles", roles));
            return ResponseEntity.ok(Map.of("accessToken", newToken));
        } catch (Exception e) {
            return ResponseEntity.status(401).body(Map.of(KEY_MESSAGE, "invalid token"));
        }
    }
}
