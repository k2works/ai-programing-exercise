package mrs;

import org.junit.jupiter.api.Test;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;

import static org.junit.jupiter.api.Assertions.*;

class PasswordHashTest {

    @Test
    void 既存のハッシュでpasswordが検証できる() {
        BCryptPasswordEncoder encoder = new BCryptPasswordEncoder();
        String password = "password";
        String existingHash = "$2a$10$mkAqkcb22hTz3bNjjGxkiOj3aRtk1FzaEamf.SlUwApbr/VYQLAKK";
        
        assertTrue(encoder.matches(password, existingHash),
            "既存のハッシュで'password'が検証できること");
    }

    @Test
    void 新しいハッシュを生成() {
        BCryptPasswordEncoder encoder = new BCryptPasswordEncoder();
        String password = "password";
        String newHash = encoder.encode(password);
        
        System.out.println("新しいBCryptハッシュ: " + newHash);
        assertTrue(encoder.matches(password, newHash));
    }
}
