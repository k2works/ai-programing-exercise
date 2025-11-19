package mrs.application.domain.model.auth;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class UserIdTest {

    @Test
    void ユーザーIDを作成できる() {
        UserId userId = new UserId("user001");
        assertEquals("user001", userId.value());
    }

    @Test
    void nullのユーザーIDは作成できない() {
        assertThrows(IllegalArgumentException.class, () -> new UserId(null));
    }

    @Test
    void 空文字のユーザーIDは作成できない() {
        assertThrows(IllegalArgumentException.class, () -> new UserId(""));
    }

    @Test
    void 同じ値のUserIdは等しい() {
        UserId userId1 = new UserId("user001");
        UserId userId2 = new UserId("user001");
        assertEquals(userId1, userId2);
        assertEquals(userId1.hashCode(), userId2.hashCode());
    }
}
