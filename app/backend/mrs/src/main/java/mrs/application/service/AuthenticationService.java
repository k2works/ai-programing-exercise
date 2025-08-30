package mrs.application.service;

import mrs.application.domain.model.auth.User;
import mrs.application.dto.LoginRequest;
import mrs.application.dto.LoginResponse;
import mrs.application.exception.AuthenticationException;
import mrs.application.port.in.AuthenticationUseCase;
import mrs.application.port.out.JwtPort;
import mrs.application.port.out.UserPort;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.stereotype.Service;

import java.util.Map;

/**
 * 認証機能のサービス実装
 * ユーザー認証とJWTトークン管理を行う
 */
@Service
@SuppressWarnings("PMD.CyclomaticComplexity") // 認証処理の性質上、条件分岐が多くなる
public class AuthenticationService implements AuthenticationUseCase {

    private final JwtPort jwtPort;
    private final BCryptPasswordEncoder passwordEncoder;
    private final UserPort userPort;

    public AuthenticationService(JwtPort jwtPort, BCryptPasswordEncoder passwordEncoder, UserPort userPort) {
        this.jwtPort = jwtPort;
        this.passwordEncoder = passwordEncoder;
        this.userPort = userPort;
    }

    @Override
    public LoginResponse authenticate(LoginRequest loginRequest) {
        validateLoginRequest(loginRequest);
        User user = validateCredentials(loginRequest);
        return createLoginResponse(user);
    }

    @Override
    public LoginResponse refreshToken(String token) {
        validateTokenNotBlank(token);

        try {
            Map<String, Object> claims = jwtPort.parseAndValidate(token);
            String userId = (String) claims.get("subject");
            Object roles = claims.get("roles");
            
            String newToken = jwtPort.createAccessToken(userId, Map.of("roles", roles));
            long expiresIn = jwtPort.getExpirationTime();
            
            return new LoginResponse(newToken, expiresIn);
        } catch (Exception e) {
            throw new AuthenticationException("Invalid or expired token", e);
        }
    }

    private void validateLoginRequest(LoginRequest loginRequest) {
        if (loginRequest.getUsername() == null || loginRequest.getUsername().isBlank() ||
            loginRequest.getPassword() == null || loginRequest.getPassword().isBlank()) {
            throw new AuthenticationException("Username and password are required");
        }
    }

    private User validateCredentials(LoginRequest loginRequest) {
        User user = userPort.findByUserId(loginRequest.getUsername());
        if (user == null || !passwordEncoder.matches(loginRequest.getPassword(), user.getPasswordHash())) {
            throw new AuthenticationException("Invalid credentials");
        }
        return user;
    }

    private LoginResponse createLoginResponse(User user) {
        String token = jwtPort.createAccessToken(user.getUserId(), Map.of("roles", user.getRole()));
        long expiresIn = jwtPort.getExpirationTime();
        return new LoginResponse(token, expiresIn);
    }

    private void validateTokenNotBlank(String token) {
        if (token == null || token.isBlank()) {
            throw new AuthenticationException("Token is required");
        }
    }
}