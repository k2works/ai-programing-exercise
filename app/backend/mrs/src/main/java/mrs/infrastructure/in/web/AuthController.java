package mrs.infrastructure.in.web;

import java.util.Map;
import mrs.application.dto.LoginRequest;
import mrs.application.dto.LoginResponse;
import mrs.common.exception.AuthenticationException;
import mrs.application.port.in.AuthenticationUseCase;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.ExampleObject;
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
    private final AuthenticationUseCase authenticationUseCase;

    public AuthController(AuthenticationUseCase authenticationUseCase) {
        this.authenticationUseCase = authenticationUseCase;
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
        @RequestBody LoginRequest loginRequest) {
        try {
            LoginResponse response = authenticationUseCase.authenticate(loginRequest);
            return ResponseEntity.ok(response);
        } catch (AuthenticationException e) {
            if (e.getMessage().contains("required")) {
                return ResponseEntity.badRequest().body(Map.of(KEY_MESSAGE, e.getMessage()));
            }
            return ResponseEntity.status(401).body(Map.of(KEY_MESSAGE, e.getMessage()));
        }
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
            LoginResponse response = authenticationUseCase.refreshToken(token);
            return ResponseEntity.ok(response);
        } catch (AuthenticationException e) {
            return ResponseEntity.status(401).body(Map.of(KEY_MESSAGE, e.getMessage()));
        }
    }
}
