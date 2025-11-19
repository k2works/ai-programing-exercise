package mrs.application.domain.model.auth;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class NameTest {

    @Test
    void 姓と名から名前を作成できる() {
        Name name = new Name("山田", "太郎");
        assertEquals("山田", name.lastName());
        assertEquals("太郎", name.firstName());
    }

    @Test
    void フルネームを取得できる() {
        Name name = new Name("山田", "太郎");
        assertEquals("山田 太郎", name.fullName());
    }

    @Test
    void nullの姓は作成できない() {
        assertThrows(IllegalArgumentException.class, () -> new Name(null, "太郎"));
    }

    @Test
    void nullの名は作成できない() {
        assertThrows(IllegalArgumentException.class, () -> new Name("山田", null));
    }

    @Test
    void 空文字の姓は作成できない() {
        assertThrows(IllegalArgumentException.class, () -> new Name("", "太郎"));
    }

    @Test
    void 空文字の名は作成できない() {
        assertThrows(IllegalArgumentException.class, () -> new Name("山田", ""));
    }
}
