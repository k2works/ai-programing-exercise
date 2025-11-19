package mrs.application.domain.model.auth;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class PasswordTest {

    @Test
    void パスワードを作成できる() {
        Password password = new Password("password123");
        assertEquals("password123", password.value());
    }

    @Test
    void nullのパスワードは作成できない() {
        assertThrows(IllegalArgumentException.class, () -> new Password(null));
    }

    @Test
    void 空文字のパスワードは作成できない() {
        assertThrows(IllegalArgumentException.class, () -> new Password(""));
    }

    @Test
    void BCrypt形式のパスワードを判定できる() {
        Password bcryptPassword = new Password("$2a$10$abcdefghijklmnopqrstuv");
        assertTrue(bcryptPassword.isBCryptEncoded());
    }

    @Test
    void 平文パスワードはBCrypt形式でないと判定される() {
        Password plainPassword = new Password("password123");
        assertFalse(plainPassword.isBCryptEncoded());
    }
}
