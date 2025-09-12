package com.k2works.mrs;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.SystemUtils;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * Apache Commons Lang3 ライブラリの動作確認テスト
 * 
 * 追加したライブラリが正常に動作することを確認する
 */
class CommonsLang3Test {
    
    @Test
    void stringUtils動作確認() {
        // StringUtils の基本的な機能をテスト
        assertThat(StringUtils.isEmpty("")).isTrue();
        assertThat(StringUtils.isEmpty(null)).isTrue();
        assertThat(StringUtils.isEmpty("test")).isFalse();
        
        assertThat(StringUtils.isBlank("  ")).isTrue();
        assertThat(StringUtils.isBlank("")).isTrue();
        assertThat(StringUtils.isBlank(null)).isTrue();
        assertThat(StringUtils.isBlank("test")).isFalse();
        
        assertThat(StringUtils.capitalize("hello")).isEqualTo("Hello");
        assertThat(StringUtils.reverse("hello")).isEqualTo("olleh");
    }
    
    @Test 
    void systemUtils動作確認() {
        // SystemUtils の基本的な情報取得をテスト
        assertThat(SystemUtils.JAVA_VERSION).isNotNull();
        assertThat(SystemUtils.OS_NAME).isNotNull();
        assertThat(SystemUtils.USER_NAME).isNotNull();
        
        // Java バージョンが期待される形式であることを確認
        assertThat(SystemUtils.JAVA_VERSION).matches("\\d+.*");
    }
    
    @Test
    void toStringBuilder動作確認() {
        // ToStringBuilder のテスト
        TestObject obj = new TestObject("test", 42);
        String result = ToStringBuilder.reflectionToString(obj);
        
        assertThat(result).contains("TestObject");
        assertThat(result).contains("name=test");
        assertThat(result).contains("value=42");
    }
    
    /**
     * テスト用のシンプルなオブジェクト
     */
    private static class TestObject {
        private final String name;
        private final int value;
        
        public TestObject(String name, int value) {
            this.name = name;
            this.value = value;
        }
    }
}