package mrs.application.domain.model.auth;

import net.jqwik.api.*;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Feature: meeting-room-reservation-system, Property 19: パスワードのBCrypt暗号化
 * 検証: 要件 8.2
 */
class PasswordPropertyTest {

    @Property
    @Label("プロパティ19: BCrypt形式のパスワードは正しく判定される")
    void bcryptPasswordsAreDetected(@ForAll("bcryptPassword") String bcryptHash) {
        Password password = new Password(bcryptHash);
        assertTrue(password.isBCryptEncoded(),
            "BCrypt形式のパスワード: " + bcryptHash);
    }

    @Property
    @Label("プロパティ19: 平文パスワードはBCrypt形式でないと判定される")
    void plainPasswordsAreNotBCrypt(@ForAll("plainPassword") String plainText) {
        Password password = new Password(plainText);
        assertFalse(password.isBCryptEncoded(),
            "平文パスワード: " + plainText);
    }

    @Provide
    Arbitrary<String> bcryptPassword() {
        // BCrypt形式: $2a$10$[22文字のsalt][31文字のhash]
        return Arbitraries.strings()
            .withCharRange('a', 'z')
            .ofLength(53)
            .map(s -> "$2a$10$" + s);
    }

    @Provide
    Arbitrary<String> plainPassword() {
        return Arbitraries.strings()
            .alpha()
            .numeric()
            .ofMinLength(8)
            .ofMaxLength(20)
            .filter(s -> !s.startsWith("$2a$") && 
                        !s.startsWith("$2b$") && 
                        !s.startsWith("$2y$"));
    }
}
