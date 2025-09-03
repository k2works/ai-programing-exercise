package mrs;

import mrs.config.TestBeansConfig;
import mrs.config.TestSecurityConfig;
import org.junit.jupiter.api.Test;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.context.annotation.Import;
import org.springframework.test.context.ActiveProfiles;

@SpringBootTest
@ActiveProfiles("test")
@Import({TestBeansConfig.class, TestSecurityConfig.class})
class ApplicationSmokeTest {
    @Test
    void contextLoads() { }
}
