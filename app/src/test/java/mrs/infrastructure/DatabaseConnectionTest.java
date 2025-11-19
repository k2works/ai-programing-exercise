package mrs.infrastructure;

import static org.assertj.core.api.Assertions.assertThat;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.jdbc.core.JdbcTemplate;

/**
 * データベース接続とマイグレーションの確認テスト
 */
@SpringBootTest
class DatabaseConnectionTest {

    @Autowired
    private JdbcTemplate jdbcTemplate;

    @Test
    void testDatabaseConnection() {
        // データベース接続が確立されていることを確認
        Integer result = jdbcTemplate.queryForObject("SELECT 1", Integer.class);
        assertThat(result).isEqualTo(1);
    }

    @Test
    void testUsrTableExists() {
        // usrテーブルが存在することを確認
        Integer count = jdbcTemplate.queryForObject(
            "SELECT COUNT(*) FROM usr", 
            Integer.class
        );
        assertThat(count).isGreaterThan(0);
    }

    @Test
    void testMeetingRoomTableExists() {
        // meeting_roomテーブルが存在することを確認
        Integer count = jdbcTemplate.queryForObject(
            "SELECT COUNT(*) FROM meeting_room", 
            Integer.class
        );
        assertThat(count).isEqualTo(5);
    }

    @Test
    void testReservableRoomTableExists() {
        // reservable_roomテーブルが存在することを確認
        Integer count = jdbcTemplate.queryForObject(
            "SELECT COUNT(*) FROM reservable_room", 
            Integer.class
        );
        // 5つの会議室 × 30日分 = 150レコード
        assertThat(count).isEqualTo(150);
    }

    @Test
    void testReservationTableExists() {
        // reservationテーブルが存在することを確認
        Integer count = jdbcTemplate.queryForObject(
            "SELECT COUNT(*) FROM reservation", 
            Integer.class
        );
        assertThat(count).isGreaterThan(0);
    }

    @Test
    void testAdminUserExists() {
        // 管理者ユーザーが存在することを確認
        String roleName = jdbcTemplate.queryForObject(
            "SELECT role_name FROM usr WHERE user_id = 'admin'", 
            String.class
        );
        assertThat(roleName).isEqualTo("ADMIN");
    }

    @Test
    void testPasswordIsEncrypted() {
        // パスワードがBCryptで暗号化されていることを確認
        String password = jdbcTemplate.queryForObject(
            "SELECT password FROM usr WHERE user_id = 'admin'", 
            String.class
        );
        assertThat(password).startsWith("$2a$");
    }
}
