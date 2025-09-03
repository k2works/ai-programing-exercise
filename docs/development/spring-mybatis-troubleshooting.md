# Spring Boot + MyBatis トラブルシューティング

## 概要

このドキュメントは、Spring BootとMyBatisを組み合わせた開発で発生する典型的な問題と解決策をまとめています。イテレーション4での実際の問題解決事例を基に作成されています。

## 問題分類と解決策

### 1. MyBatis型システム関連

#### 問題: LocalDate型エイリアスが解決できない
**エラーメッセージ**:
```
Could not resolve type alias 'localDate'
```

**原因**: MyBatisのデフォルト型エイリアスにlocalDateが含まれていない

**解決策**:
```xml
<!-- 修正前 -->
<select id="findReservableByDate" parameterType="localDate" resultMap="reservableMap">
  SELECT room_id, reservable_date FROM reservable_room WHERE reservable_date = #{date}
</select>

<!-- 修正後 -->
<select id="findReservableByDate" resultMap="reservableMap">
  SELECT room_id, reservable_date FROM reservable_room WHERE reservable_date = #{date}
</select>
```

**ベストプラクティス**:
- `parameterType`は省略可能（型推論が効く）
- 明示的に指定する場合は`jdbcType=DATE`を使用
- LocalDateのハンドリングが必要な場合は`LocalDateTypeHandler`を設定

#### 問題: 日付パラメータの型変換エラー
**解決策**:
```xml
<!-- jdbcTypeを明示的に指定 -->
<select id="findByDateRange" resultMap="resultMap">
  SELECT * FROM table WHERE date_column = #{date,jdbcType=DATE}
</select>
```

### 2. Spring Boot Test設定関連

#### 問題: Bean依存性が解決できない
**エラーメッセージ**:
```
NoSuchBeanDefinitionException: No qualifying bean of type 'BCryptPasswordEncoder'
```

**原因**: テスト実行時に必要なBeanが@SpringBootTestでは自動作成されない

**解決策**:
```java
@SpringBootTest
@ActiveProfiles("test")
@Import({TestBeansConfig.class, TestSecurityConfig.class})  // 必要なBean設定をImport
public class YourTest {
    // テスト実装
}
```

**TestBeansConfig例**:
```java
@TestConfiguration
public class TestBeansConfig {
    @Bean
    @Primary
    public BCryptPasswordEncoder passwordEncoder() {
        return new BCryptPasswordEncoder();
    }
    
    @Bean
    @Primary
    public JwtService jwtService() {
        return new JwtService();
    }
}
```

#### 問題: ApplicationContextの起動失敗
**診断手順**:
1. 依存性注入エラーを確認
2. プロファイル設定を確認
3. Bean循環依存を確認
4. 必要な設定クラスの@Importを確認

### 3. データベース関連

#### 問題: 参照整合性制約違反
**エラーメッセージ**:
```
Referential integrity constraint violation: FOREIGN KEY(user_id) REFERENCES usr(user_id)
```

**原因**: テスト実行時に参照先データが存在しない

**解決策**:
```java
@BeforeEach
void setUp() {
    // テスト用データを事前作成
    for (int i = 1; i <= REQUIRED_USERS; i++) {
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

**ベストプラクティス**:
- テストデータは@BeforeEachで作成
- 重複作成を避けるため存在チェックを実施
- @Transactionalでテスト後のクリーンアップを自動化

### 4. 並行処理テスト関連

#### 問題: 例外の分類ミス
**問題**: 期待する例外が誤ったカウンタにカウントされる

**解決策**:
```java
try {
    // 並行処理実行
    Reservation created = reservationService.reserve(reservation, user);
    if (created != null) {
        successCount.incrementAndGet();
    }
} catch (mrs.common.exception.AlreadyReservedException e) {
    // 重複予約による競合エラー（期待される例外）
    conflictCount.incrementAndGet();
} catch (IllegalStateException e) {
    // その他の状態エラー
    if (e.getMessage().contains("既に予約されています")) {
        conflictCount.incrementAndGet();
    } else {
        errorCount.incrementAndGet();
        exceptions.add(e);
    }
} catch (Exception e) {
    // 予期しないエラー
    errorCount.incrementAndGet();
    exceptions.add(e);
}
```

**ベストプラクティス**:
- 具体的な例外クラスを個別にキャッチ
- エラーメッセージでの分類は最後の手段
- デバッグ用のログを適切に出力

### 5. MyBatis XML設定関連

#### 問題: Mapperクラスとメソッドの不整合
**診断手順**:
1. namespaceがMapperインターフェースと一致するか確認
2. method idとMapperメソッド名が一致するか確認
3. parameterTypeとメソッド引数が一致するか確認
4. resultMapとreturn型が一致するか確認

**例**:
```java
// Mapperインターフェース
@Mapper
public interface UserMapper {
    void insert(User user);  // insertメソッド
    User findByUserId(@Param("userId") String userId);
}
```

```xml
<!-- 対応するXML -->
<mapper namespace="mrs.infrastructure.out.persistence.UserMapper">
  <insert id="insert">
    INSERT INTO usr (user_id, name, password_hash, role)
    VALUES (#{userId}, #{name}, #{passwordHash}, #{role})
  </insert>
  
  <select id="findByUserId" parameterType="string" resultMap="userMap">
    SELECT user_id, name, password_hash, role FROM usr WHERE user_id = #{userId}
  </select>
</mapper>
```

### 6. プロファイル設定関連

#### 問題: テスト環境での設定が効かない
**確認項目**:
```properties
# application-test.properties
spring.profiles.active=test
spring.datasource.url=jdbc:h2:mem:testdb
spring.jpa.hibernate.ddl-auto=none
spring.flyway.enabled=true
mybatis.configuration.map-underscore-to-camel-case=true
```

**テストクラスでの指定**:
```java
@ActiveProfiles("test")  // 必須
@SpringBootTest
public class YourTest {
}
```

## 診断手順

### 1. エラー分析の流れ
1. **スタックトレースの確認**: 根本原因の特定
2. **ログレベルの調整**: DEBUGレベルでMyBatis/Springの詳細ログを確認
3. **段階的テスト**: 単体→統合→負荷の順で問題を切り分け
4. **設定の確認**: プロファイル、Bean定義、XML設定の整合性確認

### 2. デバッグ用設定
```properties
# application-test.properties
logging.level.org.springframework=DEBUG
logging.level.org.mybatis=DEBUG
logging.level.mrs=DEBUG
```

### 3. トラブルシューティング用コード
```java
@BeforeEach
void setUp() {
    // ApplicationContext内のBeanを確認
    String[] beanNames = applicationContext.getBeanDefinitionNames();
    System.out.println("Available beans: " + Arrays.toString(beanNames));
    
    // データソースの確認
    DataSource dataSource = applicationContext.getBean(DataSource.class);
    System.out.println("DataSource: " + dataSource.getClass());
}
```

## 予防策

### 1. 開発時のベストプラクティス
- TypeHandlerの明示的定義
- テスト用設定クラスの標準化
- 段階的なテスト実行
- 詳細ログの活用

### 2. CI/CDでの品質ゲート
- 全テストタイプの実行
- カバレッジ測定
- 静的解析の実行
- パフォーマンステストの継続実行

### 3. ドキュメント保守
- 問題解決事例の蓄積
- 設定例の更新
- バージョンアップ時の注意事項記録

---

*この文書はイテレーション4（2025年9月3日）での実際の問題解決事例を基に作成されました。*
*新しい問題が発見された場合は、このドキュメントを更新してください。*