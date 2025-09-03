# テストガイド

## 概要

このドキュメントは、MRSプロジェクトにおけるテスト戦略、実行方法、トラブルシューティングについて説明します。

## テスト分類

### 1. 単体テスト（Unit Test）
- **実行コマンド**: `./gradlew test`
- **対象**: 個別のクラス・メソッドレベル
- **カバレッジ**: JaCoCo使用
- **レポート**: `build/reports/tests/test/index.html`

### 2. 統合テスト（Integration Test）
- **実行コマンド**: `./gradlew integrationTest`
- **対象**: コンポーネント間の連携テスト
- **データベース**: H2インメモリデータベース使用

### 3. E2Eテスト（End-to-End Test）
- **実行コマンド**: `./gradlew cucumberTest`
- **フレームワーク**: Cucumber + Spring Boot Test
- **対象**: ユーザーシナリオベースの機能テスト

### 4. パフォーマンステスト（Performance Test）
- **実行コマンド**: `./gradlew loadTest`
- **対象**: 並行処理・負荷テスト
- **クラス**: `ConcurrentReservationTest`

## パフォーマンステストの詳細

### 実行されるテスト
1. **100ユーザー同時予約テスト**: 同じ時間帯に100ユーザーが予約を試行
2. **異なる時間帯予約テスト**: 100ユーザーが異なる時間帯を予約
3. **レスポンス時間テスト**: API応答時間の計測

### 期待結果
- **同時予約**: 1件成功、99件競合エラー（AlreadyReservedException）
- **異なる時間帯**: 高い成功率（98%以上）
- **レスポンス時間**: 平均5ms以下、最大10ms以下

### 設定・依存関係
```java
@SpringBootTest
@ActiveProfiles("test")
@Import({TestBeansConfig.class, TestSecurityConfig.class})
```

必要なBean:
- `BCryptPasswordEncoder`
- `JwtService`
- `UserMapper`（テスト用ユーザー作成用）

## トラブルシューティング

### よくある問題と解決策

#### 1. MyBatis LocalDate型エラー
**エラー**: `Could not resolve type alias 'localDate'`

**解決策**:
```xml
<!-- 修正前 -->
<select id="findReservableByDate" parameterType="localDate" resultMap="reservableMap">

<!-- 修正後 -->
<select id="findReservableByDate" resultMap="reservableMap">
```

#### 2. Spring Bean依存性エラー
**エラー**: `NoSuchBeanDefinitionException: No qualifying bean of type 'BCryptPasswordEncoder'`

**解決策**:
```java
@Import({TestBeansConfig.class, TestSecurityConfig.class})
```

#### 3. 参照整合性制約違反
**エラー**: `Referential integrity constraint violation: FOREIGN KEY(user_id) REFERENCES usr(user_id)`

**解決策**: テスト実行前にテスト用ユーザーを作成
```java
@BeforeEach
void setUp() {
    // user001~user100を動的作成
    for (int i = 1; i <= CONCURRENT_USERS; i++) {
        String userId = String.format("user%03d", i);
        if (userMapper.findByUserId(userId) == null) {
            User user = new User();
            user.setUserId(userId);
            user.setName("Test User " + i);
            user.setPasswordHash("dummy_hash");
            user.setRole("USER");
            userMapper.insert(user);
        }
    }
}
```

#### 4. 例外処理の分類ミス
**問題**: `AlreadyReservedException`が`errorCount`にカウントされる

**解決策**:
```java
} catch (mrs.common.exception.AlreadyReservedException e) {
    // 重複予約による競合エラー
    conflictCount.incrementAndGet();
} catch (IllegalStateException e) {
    // その他の状態エラー
    if (e.getMessage().contains("既に予約されています")) {
        conflictCount.incrementAndGet();
    } else {
        errorCount.incrementAndGet();
    }
}
```

## テスト実行環境

### 必要な設定
- **プロファイル**: `test`
- **データベース**: H2インメモリ
- **Flyway**: テーブル初期化
- **Spring Security**: テスト用設定

### 環境変数
```properties
spring.profiles.active=test
spring.datasource.url=jdbc:h2:mem:testdb
spring.jpa.hibernate.ddl-auto=none
spring.flyway.enabled=true
```

## CI/CD統合

### 品質ゲート
- **単体テスト**: 100%成功必須
- **統合テスト**: 100%成功必須
- **パフォーマンステスト**: 100%成功推奨
- **コードカバレッジ**: 80%以上推奨

### レポート生成
```bash
# 全テスト実行＋レポート生成
./gradlew qualityCheck

# 個別実行
./gradlew test integrationTest cucumberTest loadTest
```

## ベストプラクティス

### テストデータ管理
1. **@BeforeEach**でテストデータをセットアップ
2. **@AfterEach**または**@Transactional**でクリーンアップ
3. テスト間の独立性を保つ

### パフォーマンステスト
1. **CountDownLatch**で並行実行の同期制御
2. **AtomicInteger**でスレッドセーフなカウンタ
3. **詳細ログ**で問題分析を容易に

### エラーハンドリング
1. **具体的な例外クラス**を個別にキャッチ
2. **エラーメッセージ**に基づく分類
3. **デバッグ情報**の適切な出力

## メンテナンス

### 定期的な確認項目
- [ ] 全テストの実行時間が許容範囲内
- [ ] テストデータの整合性
- [ ] 例外処理の適切性
- [ ] レポート生成の正常性

### アップデート時の注意
1. Spring Boot / MyBatisバージョン更新時は型システムを確認
2. 新機能追加時はテスト用データの更新を検討
3. パフォーマンス要件の見直し

---

*このガイドはイテレーション4（2025年9月3日）の成果を反映しています*