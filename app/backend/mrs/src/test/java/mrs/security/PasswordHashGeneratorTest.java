package mrs.security;

import static org.junit.jupiter.api.Assertions.assertTrue;
import org.junit.jupiter.api.Test;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;

public class PasswordHashGeneratorTest {
    @Test
    void generateAndVerifyDemoHash() {
        BCryptPasswordEncoder enc = new BCryptPasswordEncoder();
        String hash = enc.encode("demo");
        assertTrue(enc.matches("demo", hash));
    }
}
