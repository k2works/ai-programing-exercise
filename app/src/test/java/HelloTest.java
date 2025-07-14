import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.Test;

class HelloTest {
  @Test
  void test_greeting() {
    assertEquals("hello world", greeting());
  }

  private String greeting() {
    return "hello world";
  }
}
