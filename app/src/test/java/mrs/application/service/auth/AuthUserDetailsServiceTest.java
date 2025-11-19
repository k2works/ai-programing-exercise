package mrs.application.service.auth;

import mrs.application.domain.model.auth.*;
import mrs.application.port.out.UserPort;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UsernameNotFoundException;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

class AuthUserDetailsServiceTest {
    private UserPort userPort;
    private AuthUserDetailsService service;

    @BeforeEach
    void setUp() {
        userPort = mock(UserPort.class);
        service = new AuthUserDetailsService(userPort);
    }

    @Test
    void 有効なユーザーIDで認証情報を取得できる() {
        User user = new User(
            new UserId("user001"),
            new Password("$2a$10$abcdefghijklmnopqrstuv"),
            new Name("山田", "太郎"),
            RoleName.USER
        );
        when(userPort.findById(new UserId("user001"))).thenReturn(user);

        UserDetails userDetails = service.loadUserByUsername("user001");

        assertNotNull(userDetails);
        assertEquals("user001", userDetails.getUsername());
        assertTrue(userDetails.getAuthorities().stream()
            .anyMatch(auth -> auth.getAuthority().equals("ROLE_USER")));
    }

    @Test
    void 存在しないユーザーIDで例外が発生する() {
        when(userPort.findById(new UserId("invalid"))).thenReturn(null);

        assertThrows(UsernameNotFoundException.class, 
            () -> service.loadUserByUsername("invalid"));
    }

    @Test
    void 管理者ユーザーの権限が正しく設定される() {
        User admin = new User(
            new UserId("admin"),
            new Password("$2a$10$abcdefghijklmnopqrstuv"),
            new Name("管理", "太郎"),
            RoleName.ADMIN
        );
        when(userPort.findById(new UserId("admin"))).thenReturn(admin);

        UserDetails userDetails = service.loadUserByUsername("admin");

        assertTrue(userDetails.getAuthorities().stream()
            .anyMatch(auth -> auth.getAuthority().equals("ROLE_ADMIN")));
    }
}
