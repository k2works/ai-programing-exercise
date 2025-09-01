package mrs.application.service;

import mrs.application.domain.model.auth.User;
import mrs.application.dto.LoginRequest;
import mrs.application.dto.LoginResponse;
import mrs.common.exception.AuthenticationException;
import mrs.application.port.out.JwtPort;
import mrs.application.port.out.UserPort;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;

import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class AuthenticationServiceTest {
    
    static {
        // Disable Spring Boot during unit tests
        System.setProperty("spring.main.web-application-type", "none");
        System.setProperty("spring.autoconfigure.exclude", "org.springframework.boot.autoconfigure.jdbc.DataSourceAutoConfiguration");
    }

    @Mock
    private JwtPort jwtPort;

    @Mock
    private BCryptPasswordEncoder passwordEncoder;

    @Mock
    private UserPort userPort;

    @InjectMocks
    private AuthenticationService authenticationService;

    private User testUser;
    private LoginRequest testLoginRequest;

    @BeforeEach
    void setUp() {
        testUser = new User();
        testUser.setUserId("user001");
        testUser.setName("田中太郎");
        testUser.setPasswordHash("$2a$10$hashedPassword");
        testUser.setRole("USER");

        testLoginRequest = new LoginRequest();
        testLoginRequest.setUsername("user001");
        testLoginRequest.setPassword("password123");
    }

    @Test
    void testAuthenticate_成功() {
        // Arrange
        when(userPort.findByUserId(anyString())).thenReturn(testUser);
        when(passwordEncoder.matches(anyString(), anyString())).thenReturn(true);
        when(jwtPort.createAccessToken(anyString(), any(Map.class))).thenReturn("jwt-token");
        when(jwtPort.getExpirationTime()).thenReturn(3600L);

        // Act
        try {
            LoginResponse response = authenticationService.authenticate(testLoginRequest);
            
            // Assert
            assertNotNull(response);
            assertEquals("jwt-token", response.getToken());
            assertEquals(3600L, response.getExpiresIn());
        } catch (Exception e) {
            fail("Authentication should succeed, but got exception: " + e.getMessage());
        }
    }

    @Test
    void testAuthenticate_ユーザー名がnull() {
        // Arrange
        testLoginRequest.setUsername(null);

        // Act & Assert
        AuthenticationException exception = assertThrows(
                AuthenticationException.class,
                () -> authenticationService.authenticate(testLoginRequest)
        );
        assertEquals("Username and password are required", exception.getMessage());
    }
}