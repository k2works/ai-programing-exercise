package com.k2works.aipe;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class SmokeUnitTest {

    @Test
    @DisplayName("JUnit 5 のテスト基盤が動作する")
    void junitPlatformIsAvailable() {
        assertEquals(2, 1 + 1);
    }
}
