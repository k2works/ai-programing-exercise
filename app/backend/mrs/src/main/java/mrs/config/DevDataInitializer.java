package mrs.config;

import org.springframework.boot.ApplicationArguments;
import org.springframework.boot.ApplicationRunner;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.stereotype.Component;
import edu.umd.cs.findbugs.annotations.SuppressFBWarnings;

@Component
@SuppressFBWarnings(value = "EI2", justification = "Spring-managed beans (JdbcTemplate, Encoder) are immutable service references here")
public class DevDataInitializer implements ApplicationRunner {
    private final JdbcTemplate jdbcTemplate;
    private final BCryptPasswordEncoder encoder;

    public DevDataInitializer(JdbcTemplate jdbcTemplate, BCryptPasswordEncoder encoder) {
        this.jdbcTemplate = jdbcTemplate;
        this.encoder = encoder;
    }

    @Override
    public void run(ApplicationArguments args) {
        // Always ensure user1 has a valid bcrypt for "demo" in dev
        String hash = encoder.encode("demo");
        jdbcTemplate.update("UPDATE usr SET password_hash=? WHERE user_id=?", hash, "user1");
    }
}
