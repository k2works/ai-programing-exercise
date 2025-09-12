package com.k2works.mrs;

import org.junit.jupiter.api.Test;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.ActiveProfiles;

@SpringBootTest
@ActiveProfiles("test")
class MrsApplicationTests {

	@Test
	void contextLoads() {
		// Spring Context が正常にロードされることをテスト
	}

}