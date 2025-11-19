package mrs.application.domain.model.auth;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class UserTest {

    @Test
    void ユーザーを作成できる() {
        UserId userId = new UserId("user001");
        Password password = new Password("password123");
        Name name = new Name("山田", "太郎");
        RoleName roleName = RoleName.USER;

        User user = new User(userId, password, name, roleName);

        assertEquals(userId, user.userId());
        assertEquals(password, user.password());
        assertEquals(name, user.name());
        assertEquals(roleName, user.roleName());
    }

    @Test
    void 管理者ユーザーはisAdminがtrueを返す() {
        User admin = new User(
            new UserId("admin"),
            new Password("password"),
            new Name("管理", "太郎"),
            RoleName.ADMIN
        );

        assertTrue(admin.isAdmin());
    }

    @Test
    void 一般ユーザーはisAdminがfalseを返す() {
        User user = new User(
            new UserId("user001"),
            new Password("password"),
            new Name("山田", "太郎"),
            RoleName.USER
        );

        assertFalse(user.isAdmin());
    }
}
