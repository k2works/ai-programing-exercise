# Meeting Room Reservation System - 実装タスク計画書

## 実装概要

既存のPhase 1基盤（ヘキサゴナルアーキテクチャ + Spring Boot + Docker環境）を活用し、4週間で本番レベルの会議室予約システムを段階的に実装します。

### 基盤確認（Phase 1完了済み）
- ✅ **Gradle プロジェクト**: Spring Boot 3.3.2 + Java 21
- ✅ **ヘキサゴナルアーキテクチャ**: 4層レイヤー構造 + ArchUnit制約
- ✅ **Docker環境**: PostgreSQL + Redis + アプリケーション
- ✅ **データベース**: 5つのFlywayマイグレーション
- ✅ **品質管理**: PMD、Checkstyle、SpotBugs、JaCoCo 90%

## 実装戦略

### セキュリティファースト開発
- OWASP Top 10対策の段階的組み込み
- JWT + Spring Security完全統合
- 監査ログ・アクセス制御の全自動化

### テスト駆動開発（TDD）
- Red-Green-Refactor サイクル徹底
- ピラミッド形テスト（Unit 70% + Integration 25% + E2E 5%）
- Cucumber BDD受け入れテスト

### 継続的品質管理
- コミット毎：静的解析 + ユニットテスト
- PR毎：統合テスト + セキュリティスキャン + アーキテクチャ検証
- 週次：E2Eテスト + パフォーマンステスト

## Week 1: ドメイン基盤 + 認証システム

### Day 1-2: ドメインモデル実装

#### タスク 1.1: 予約集約ルート実装（8h）

**TDD サイクル**:
```java
// RED: 失敗するテスト
@Test
void 予約は営業時間内のみ作成可能() {
    // Given
    ReservationPeriod invalidPeriod = new ReservationPeriod(
        LocalDate.now(), 
        LocalTime.of(8, 0),  // 営業時間外
        LocalTime.of(10, 0)
    );
    
    // When & Then
    assertThrows(IllegalArgumentException.class, () -> {
        new Reservation(user, meetingRoom, invalidPeriod, "テスト", 5, "");
    });
}
```

**実装ファイル**:
```
app/src/main/java/com/example/meetingroomreservation/domain/reservation/
├── Reservation.java                    # 予約集約ルート
├── ReservationId.java                  # 値オブジェクト
├── ReservationPeriod.java             # 値オブジェクト
├── ReservationStatus.java             # 列挙型
└── ReservationRepository.java         # リポジトリ インターフェース
```

**実装内容**:
- 予約集約ルートの完全実装
- 営業時間制約（9:00-22:00）
- 最大予約時間制約（6時間）
- 予約取消ルール（2時間前まで）
- 定員チェック機能

**完了基準**:
- [ ] ユニットテスト 100%パス（15個以上のテストケース）
- [ ] ビジネスルール制約の完全実装
- [ ] 値オブジェクトの不変性保証
- [ ] ArchUnit制約チェック通過

---

#### タスク 1.2: ユーザー・会議室エンティティ実装（6h）

**実装ファイル**:
```
app/src/main/java/com/example/meetingroomreservation/domain/
├── user/
│   ├── User.java                      # ユーザーエンティティ
│   ├── UserId.java                    # 値オブジェクト
│   ├── UserRole.java                  # 列挙型
│   └── UserRepository.java           # リポジトリ インターフェース
└── meetingroom/
    ├── MeetingRoom.java               # 会議室エンティティ
    ├── MeetingRoomId.java             # 値オブジェクト
    ├── RoomCapacity.java              # 値オブジェクト
    └── MeetingRoomRepository.java     # リポジトリ インターフェース
```

**完了基準**:
- [ ] エンティティの完全実装
- [ ] ドメイン不変条件の実装
- [ ] ユニットテスト 90%以上カバレッジ

---

### Day 3-4: 認証・認可システム実装

#### タスク 1.3: JWT認証基盤実装（10h）

**実装ファイル**:
```
app/src/main/java/com/example/meetingroomreservation/infrastructure/security/
├── JwtAuthenticationProvider.java      # JWT認証プロバイダー
├── JwtTokenService.java                # JWT生成・検証サービス
├── JwtAuthenticationFilter.java        # JWT認証フィルター
├── SecurityConfiguration.java          # Spring Security設定
└── CustomUserDetailsService.java      # ユーザー詳細サービス
```

**セキュリティベストプラクティス自動適用**:
```java
@Configuration
@EnableWebSecurity
public class SecurityConfiguration {
    
    @Bean
    public SecurityFilterChain filterChain(HttpSecurity http) throws Exception {
        return http
            .csrf(csrf -> csrf.disable()) // APIなのでCSRF無効
            .sessionManagement(session -> 
                session.sessionCreationPolicy(SessionCreationPolicy.STATELESS))
            .authorizeHttpRequests(authz -> authz
                .requestMatchers("/api/auth/**").permitAll()
                .requestMatchers("/api/public/**").permitAll()
                .requestMatchers(HttpMethod.POST, "/api/reservations").hasAnyRole("MEMBER", "STAFF")
                .requestMatchers("/api/admin/**").hasRole("ADMIN")
                .anyRequest().authenticated()
            )
            .addFilterBefore(jwtAuthenticationFilter(), UsernamePasswordAuthenticationFilter.class)
            .exceptionHandling(ex -> ex
                .authenticationEntryPoint(customAuthenticationEntryPoint())
                .accessDeniedHandler(customAccessDeniedHandler())
            )
            .headers(headers -> headers
                .frameOptions().deny()
                .contentTypeOptions().and()
                .httpStrictTransportSecurity(hstsConfig -> hstsConfig
                    .maxAgeInSeconds(31536000)
                    .includeSubdomains(true)
                )
            )
            .build();
    }
}
```

**完了基準**:
- [ ] JWT生成・検証機能の完全実装
- [ ] ロールベースアクセス制御（RBAC）実装
- [ ] セキュリティヘッダー自動設定
- [ ] OWASP Top 10対策実装
- [ ] セキュリティテスト 100%パス

---

#### タスク 1.4: 監査ログシステム実装（6h）

**実装ファイル**:
```
app/src/main/java/com/example/meetingroomreservation/infrastructure/audit/
├── AuditLogService.java               # 監査ログサービス
├── AuditLogRepository.java            # 監査ログリポジトリ
├── AuditAspect.java                   # AOP監査アスペクト
└── AuditLogEntity.java                # 監査ログエンティティ
```

**完了基準**:
- [ ] 全API操作の自動ログ記録
- [ ] セキュリティイベントの完全追跡
- [ ] 6ヶ月自動ローテーション実装

---

### Day 5: ドメインサービス実装

#### タスク 1.5: 予約ドメインサービス実装（8h）

**実装ファイル**:
```
app/src/main/java/com/example/meetingroomreservation/domain/reservation/
├── ReservationDomainService.java      # 予約ドメインサービス
├── AvailabilityChecker.java           # 空室チェッカー
├── ReservationPolicy.java             # 予約ポリシー
└── ConflictDetector.java              # 競合検出サービス
```

**ビジネスロジック実装**:
```java
@Service
public class ReservationDomainService {
    
    public boolean isRoomAvailable(Long roomId, ReservationPeriod period) {
        List<Reservation> conflicts = reservationRepository
            .findConflictingReservations(roomId, period);
        return conflicts.isEmpty();
    }
    
    public void validateReservationConstraints(User user, MeetingRoom room, ReservationRequest request) {
        // 定員チェック
        if (request.getAttendeeCount() > room.getCapacity()) {
            throw new ExceedsRoomCapacityException();
        }
        
        // 同時予約数制限
        long activeReservations = reservationRepository.countActiveReservationsByUser(user.getId());
        if (activeReservations >= 3) {
            throw new ExceedsMaxReservationsException();
        }
        
        // 予約期限チェック
        validateReservationDeadline(request);
    }
}
```

**完了基準**:
- [ ] ビジネスルール完全実装
- [ ] 同時アクセス制御（悲観的ロック）
- [ ] ドメインテスト 95%以上カバレッジ

---

## Week 2: アプリケーション層 + リポジトリ実装

### Day 6-7: JPA リポジトリ実装

#### タスク 2.1: リポジトリ実装（12h）

**実装ファイル**:
```
app/src/main/java/com/example/meetingroomreservation/infrastructure/persistence/
├── jpa/
│   ├── JpaReservationRepository.java  # JPA予約リポジトリ
│   ├── JpaUserRepository.java         # JPAユーザーリポジトリ
│   ├── JpaMeetingRoomRepository.java  # JPA会議室リポジトリ
│   └── JpaInquiryRepository.java      # JPA問い合わせリポジトリ
├── entity/
│   ├── ReservationJpaEntity.java      # JPA予約エンティティ
│   ├── UserJpaEntity.java             # JPAユーザーエンティティ
│   ├── MeetingRoomJpaEntity.java      # JPA会議室エンティティ
│   └── InquiryJpaEntity.java          # JPA問い合わせエンティティ
└── converter/
    ├── ReservationEntityConverter.java # ドメイン⇔JPA変換
    ├── UserEntityConverter.java        # ドメイン⇔JPA変換
    └── MeetingRoomEntityConverter.java # ドメイン⇔JPA変換
```

**性能最適化クエリ実装**:
```java
@Repository
public class JpaReservationRepository implements ReservationRepository {
    
    @Query("""
        SELECT r FROM ReservationJpaEntity r 
        WHERE r.meetingRoomId = :roomId 
        AND r.reservationDate = :date 
        AND r.status IN ('CONFIRMED', 'PENDING')
        AND (
            (r.startTime < :endTime AND r.endTime > :startTime)
        )
        """)
    List<ReservationJpaEntity> findConflictingReservations(
        @Param("roomId") Long roomId,
        @Param("date") LocalDate date,
        @Param("startTime") LocalTime startTime,
        @Param("endTime") LocalTime endTime
    );
    
    @Lock(LockModeType.PESSIMISTIC_WRITE)
    @Query("SELECT r FROM ReservationJpaEntity r WHERE r.id = :id")
    Optional<ReservationJpaEntity> findByIdForUpdate(@Param("id") Long id);
}
```

**完了基準**:
- [ ] 全リポジトリインターフェース実装完了
- [ ] 性能最適化クエリ実装
- [ ] TestContainers統合テスト 100%パス
- [ ] 悲観的ロック実装・テスト完了

---

### Day 8-9: ユースケース実装

#### タスク 2.2: 予約管理ユースケース実装（12h）

**実装ファイル**:
```
app/src/main/java/com/example/meetingroomreservation/application/
├── usecase/
│   ├── ReserveRoomUseCase.java        # 予約作成ユースケース
│   ├── CancelReservationUseCase.java  # 予約取消ユースケース
│   ├── SearchAvailableRoomsUseCase.java # 空室検索ユースケース
│   └── GetReservationHistoryUseCase.java # 予約履歴取得
├── dto/
│   ├── ReservationRequest.java        # 予約リクエストDTO
│   ├── ReservationResponse.java       # 予約レスポンスDTO
│   ├── RoomSearchCriteria.java        # 検索条件DTO
│   └── RoomSearchResponse.java        # 検索結果DTO
└── port/
    ├── NotificationPort.java           # 通知ポート
    └── CachePort.java                  # キャッシュポート
```

**ユースケース実装例**:
```java
@Service
@Transactional
public class ReserveRoomUseCase {
    
    public ReservationResponse execute(ReservationRequest request) {
        // 1. エンティティ取得・検証
        User user = userRepository.findById(request.getUserId())
            .orElseThrow(() -> new UserNotFoundException());
        MeetingRoom room = meetingRoomRepository.findById(request.getRoomId())
            .orElseThrow(() -> new MeetingRoomNotFoundException());
        
        // 2. 予約期間作成・ビジネスルール検証
        ReservationPeriod period = new ReservationPeriod(
            request.getReservationDate(),
            request.getStartTime(), 
            request.getEndTime()
        );
        
        // 3. ドメインサービスによる制約チェック
        reservationDomainService.validateReservationConstraints(user, room, request);
        
        // 4. 空室チェック（悲観的ロック）
        if (!reservationDomainService.isRoomAvailable(room.getId(), period)) {
            throw new RoomNotAvailableException();
        }
        
        // 5. 予約作成・保存
        Reservation reservation = new Reservation(user, room, period, 
            request.getPurpose(), request.getAttendeeCount(), request.getNotes());
        Reservation saved = reservationRepository.save(reservation);
        
        // 6. 通知送信（非同期）
        notificationPort.sendReservationConfirmation(saved);
        
        // 7. キャッシュ無効化
        cachePort.evictRoomAvailability(room.getId());
        
        return ReservationResponse.from(saved);
    }
}
```

**完了基準**:
- [ ] 5つの主要ユースケース完全実装
- [ ] トランザクション境界適切設定
- [ ] 例外ハンドリング完全実装
- [ ] 統合テスト 90%以上カバレッジ

---

### Day 10: エラーハンドリング + トランザクション

#### タスク 2.3: 包括的エラーハンドリング実装（8h）

**実装ファイル**:
```
app/src/main/java/com/example/meetingroomreservation/
├── domain/exception/
│   ├── BusinessException.java          # ビジネス例外基底クラス
│   ├── ReservationException.java       # 予約関連例外
│   ├── UserException.java             # ユーザー関連例外
│   └── MeetingRoomException.java      # 会議室関連例外
└── presentation/exception/
    ├── GlobalExceptionHandler.java     # グローバル例外ハンドラー
    ├── ErrorResponse.java              # エラーレスポンスDTO
    └── ErrorCode.java                  # エラーコード定義
```

**グローバルエラーハンドリング**:
```java
@RestControllerAdvice
public class GlobalExceptionHandler {
    
    @ExceptionHandler(BusinessException.class)
    public ResponseEntity<ErrorResponse> handleBusinessException(BusinessException e) {
        ErrorResponse error = ErrorResponse.builder()
            .code(e.getErrorCode())
            .message(e.getMessage())
            .timestamp(LocalDateTime.now())
            .build();
        
        // 監査ログ記録
        auditLogService.logError(e);
        
        return ResponseEntity.badRequest().body(error);
    }
    
    @ExceptionHandler(AccessDeniedException.class) 
    public ResponseEntity<ErrorResponse> handleAccessDenied(AccessDeniedException e) {
        // セキュリティ違反として記録
        securityAuditService.logUnauthorizedAccess(e);
        
        return ResponseEntity.status(HttpStatus.FORBIDDEN)
            .body(ErrorResponse.forbidden("アクセスが拒否されました"));
    }
}
```

**完了基準**:
- [ ] 包括的例外階層実装
- [ ] セキュリティ例外の適切処理
- [ ] エラーメッセージの国際化対応
- [ ] エラーハンドリングテスト完了

---

## Week 3: REST API + 統合テスト

### Day 11-12: REST Controller実装

#### タスク 3.1: REST API実装（12h）

**実装ファイル**:
```
app/src/main/java/com/example/meetingroomreservation/presentation/
├── controller/
│   ├── ReservationController.java     # 予約API
│   ├── MeetingRoomController.java     # 会議室API  
│   ├── UserController.java            # ユーザー管理API
│   ├── InquiryController.java         # 問い合わせAPI
│   └── AuthController.java            # 認証API
├── dto/
│   ├── request/                       # リクエストDTO
│   └── response/                      # レスポンスDTO
└── validator/
    ├── ReservationValidator.java      # 予約バリデーター
    └── RoomSearchValidator.java       # 検索バリデーター
```

**REST API実装例**:
```java
@RestController
@RequestMapping("/api/reservations")
@PreAuthorize("hasAnyRole('MEMBER', 'STAFF', 'ADMIN')")
@Validated
public class ReservationController {
    
    @PostMapping
    @PreAuthorize("hasAnyRole('MEMBER', 'STAFF')")
    public ResponseEntity<ReservationResponse> createReservation(
            @Valid @RequestBody ReservationRequest request,
            Authentication authentication) {
        
        // 認証ユーザーIDを設定
        request.setUserId(getCurrentUserId(authentication));
        
        ReservationResponse response = reserveRoomUseCase.execute(request);
        
        return ResponseEntity.status(HttpStatus.CREATED).body(response);
    }
    
    @DeleteMapping("/{reservationId}")
    @PreAuthorize("hasAnyRole('MEMBER', 'STAFF') and @reservationSecurityService.canCancelReservation(#reservationId, authentication)")
    public ResponseEntity<Void> cancelReservation(
            @PathVariable Long reservationId,
            Authentication authentication) {
        
        cancelReservationUseCase.execute(reservationId);
        
        return ResponseEntity.noContent().build();
    }
    
    @GetMapping("/search")
    public ResponseEntity<RoomSearchResponse> searchAvailableRooms(
            @Valid @ModelAttribute RoomSearchCriteria criteria) {
        
        RoomSearchResponse response = searchAvailableRoomsUseCase.execute(criteria);
        
        return ResponseEntity.ok(response);
    }
}
```

**完了基準**:
- [ ] 全APIエンドポイント実装完了
- [ ] OpenAPI 3.0仕様自動生成
- [ ] リクエスト・レスポンスバリデーション完全実装
- [ ] セキュリティ制約適用完了

---

#### タスク 3.2: OpenAPI + バリデーション実装（6h）

**OpenAPI設定**:
```java
@Configuration
public class OpenApiConfiguration {
    
    @Bean
    public OpenAPI customOpenAPI() {
        return new OpenAPI()
            .info(new Info()
                .title("Meeting Room Reservation API")
                .version("1.0.0")
                .description("会議室予約システム REST API仕様"))
            .addSecurityItem(new SecurityRequirement().addList("bearerAuth"))
            .components(new Components()
                .addSecuritySchemes("bearerAuth", 
                    new SecurityScheme()
                        .type(SecurityScheme.Type.HTTP)
                        .scheme("bearer")
                        .bearerFormat("JWT")));
    }
}
```

**完了基準**:
- [ ] Swagger UI自動生成
- [ ] API仕様書完全性確認
- [ ] バリデーションルール網羅実装

---

### Day 13-14: 統合・E2Eテスト

#### タスク 3.3: 統合テスト実装（12h）

**TestContainers統合テスト**:
```java
@SpringBootTest
@Testcontainers
class ReservationIntegrationTest {
    
    @Container
    static PostgreSQLContainer<?> postgres = new PostgreSQLContainer<>("postgres:16")
            .withDatabaseName("testdb")
            .withUsername("test")
            .withPassword("test");
            
    @Container
    static GenericContainer<?> redis = new GenericContainer<>("redis:7-alpine")
            .withExposedPorts(6379);
    
    @DynamicPropertySource
    static void configureProperties(DynamicPropertyRegistry registry) {
        registry.add("spring.datasource.url", postgres::getJdbcUrl);
        registry.add("spring.datasource.username", postgres::getUsername);
        registry.add("spring.datasource.password", postgres::getPassword);
        registry.add("spring.redis.host", redis::getHost);
        registry.add("spring.redis.port", redis::getFirstMappedPort);
    }
    
    @Test
    @Transactional
    void 予約作成から取消まで全フロー正常動作() {
        // Given: ユーザー・会議室データ準備
        
        // When: 予約作成API呼び出し
        
        // Then: 予約確定・通知送信確認
        
        // When: 予約取消API呼び出し
        
        // Then: 予約取消・キャッシュ無効化確認
    }
}
```

**Cucumber BDD統合テスト**:
```gherkin
Feature: 会議室予約システム
  
  Scenario: 会員が会議室を予約する
    Given 会員"yamada@example.com"でログインしている
    And 会議室"A101"が利用可能である
    When 明日の10:00-12:00に会議室"A101"を予約する
    Then 予約が正常に作成される
    And 予約確認メールが送信される
    
  Scenario: 重複予約の防止
    Given 会議室"A101"が明日の10:00-12:00に予約済み
    When 別の会員が同じ時間帯に予約を試行する
    Then 予約が拒否される
    And "指定時間帯は予約済み"エラーが返される
```

**完了基準**:
- [ ] 主要業務フロー統合テスト完了
- [ ] TestContainers環境構築完了  
- [ ] Cucumber BDD受け入れテスト実装
- [ ] テストカバレッジ目標達成（統合テスト 25%）

---

### Day 15: セキュリティ・パフォーマンステスト

#### タスク 3.4: セキュリティテスト実装（8h）

**セキュリティテスト**:
```java
@SpringBootTest
@AutoConfigureMockMvc
class SecurityTest {
    
    @Test
    void JWT認証なしのAPIアクセスは401エラー() {
        mockMvc.perform(get("/api/reservations"))
            .andExpect(status().isUnauthorized());
    }
    
    @Test
    void 無効なJWTトークンは401エラー() {
        mockMvc.perform(get("/api/reservations")
                .header("Authorization", "Bearer invalid-token"))
            .andExpected(status().isUnauthorized());
    }
    
    @Test
    void 権限不足のAPIアクセスは403エラー() {
        String memberToken = generateJwtToken("member@example.com", "MEMBER");
        
        mockMvc.perform(delete("/api/admin/users/1")
                .header("Authorization", "Bearer " + memberToken))
            .andExpect(status().isForbidden());
    }
    
    @Test
    void SQLインジェクション攻撃の防止() {
        String maliciousInput = "'; DROP TABLE users; --";
        
        mockMvc.perform(get("/api/rooms/search")
                .param("location", maliciousInput))
            .andExpect(status().isBadRequest())
            .andExpect(jsonPath("$.code").value("VALIDATION_ERROR"));
    }
}
```

**完了基準**:
- [ ] OWASP Top 10対策検証完了
- [ ] JWT セキュリティテスト完了
- [ ] SQL インジェクション対策確認
- [ ] XSS・CSRF 対策検証完了

---

## Week 4: フロントエンド + 最終統合

### Day 16: React アプリケーション基盤

#### タスク 4.1: React プロジェクト初期化（8h）

**プロジェクト構造**:
```
frontend/
├── public/
├── src/
│   ├── components/         # 再利用可能コンポーネント
│   ├── pages/             # ページコンポーネント
│   ├── hooks/             # カスタムフック
│   ├── services/          # API クライアント
│   ├── store/             # Redux 状態管理
│   ├── types/             # TypeScript 型定義
│   ├── utils/             # ユーティリティ関数
│   └── __tests__/         # テストファイル
├── package.json
└── vite.config.ts
```

**API クライアント実装**:
```typescript
// services/apiClient.ts
import axios, { AxiosInstance, AxiosResponse } from 'axios';

class ApiClient {
  private client: AxiosInstance;

  constructor(baseURL: string) {
    this.client = axios.create({
      baseURL,
      timeout: 10000,
    });

    // JWT トークン自動付与
    this.client.interceptors.request.use((config) => {
      const token = localStorage.getItem('accessToken');
      if (token) {
        config.headers.Authorization = `Bearer ${token}`;
      }
      return config;
    });

    // レスポンスエラーハンドリング
    this.client.interceptors.response.use(
      (response) => response,
      async (error) => {
        if (error.response?.status === 401) {
          // トークンリフレッシュ処理
          await this.refreshToken();
        }
        return Promise.reject(error);
      }
    );
  }

  async createReservation(request: CreateReservationRequest): Promise<ReservationResponse> {
    const response = await this.client.post<ReservationResponse>('/reservations', request);
    return response.data;
  }

  async searchRooms(criteria: RoomSearchCriteria): Promise<RoomSearchResponse> {
    const response = await this.client.get<RoomSearchResponse>('/rooms/search', {
      params: criteria
    });
    return response.data;
  }
}
```

**完了基準**:
- [ ] React + TypeScript プロジェクト構築
- [ ] API クライアント実装完了
- [ ] 認証機能実装完了
- [ ] ルーティング設定完了

---

### Day 17-18: 主要コンポーネント実装

#### タスク 4.2: 予約関連コンポーネント実装（12h）

**コンポーネント実装**:
```typescript
// components/ReservationForm.tsx
interface ReservationFormProps {
  availableRooms: MeetingRoom[];
  onSubmit: (request: CreateReservationRequest) => Promise<void>;
  loading?: boolean;
}

export const ReservationForm: React.FC<ReservationFormProps> = ({
  availableRooms,
  onSubmit,
  loading = false
}) => {
  const [formData, setFormData] = useState<CreateReservationRequest>({
    roomId: 0,
    reservationDate: '',
    startTime: '',
    endTime: '',
    purpose: '',
    attendeeCount: 1
  });

  const [errors, setErrors] = useState<Record<string, string>>({});

  const validateForm = (): boolean => {
    const newErrors: Record<string, string> = {};

    if (!formData.roomId) newErrors.roomId = '会議室を選択してください';
    if (!formData.reservationDate) newErrors.reservationDate = '日付を選択してください';
    if (!formData.startTime) newErrors.startTime = '開始時間を選択してください';
    if (!formData.endTime) newErrors.endTime = '終了時間を選択してください';
    if (!formData.purpose.trim()) newErrors.purpose = '利用目的を入力してください';
    if (formData.attendeeCount < 1) newErrors.attendeeCount = '参加者数は1人以上である必要があります';

    // 時間の妥当性チェック
    if (formData.startTime >= formData.endTime) {
      newErrors.endTime = '終了時間は開始時間より後である必要があります';
    }

    setErrors(newErrors);
    return Object.keys(newErrors).length === 0;
  };

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    
    if (!validateForm()) return;

    try {
      await onSubmit(formData);
    } catch (error) {
      console.error('Reservation failed:', error);
    }
  };

  return (
    <form onSubmit={handleSubmit} className="reservation-form">
      {/* フォーム実装 */}
    </form>
  );
};
```

**完了基準**:
- [ ] 予約フォームコンポーネント完成
- [ ] 会議室検索コンポーネント完成  
- [ ] 予約一覧コンポーネント完成
- [ ] レスポンシブデザイン対応完了

---

#### タスク 4.3: 状態管理実装（8h）

**Redux Store実装**:
```typescript
// store/reservationSlice.ts
import { createSlice, createAsyncThunk } from '@reduxjs/toolkit';

export const createReservation = createAsyncThunk(
  'reservation/create',
  async (request: CreateReservationRequest, { rejectWithValue }) => {
    try {
      const response = await apiClient.createReservation(request);
      return response;
    } catch (error: any) {
      return rejectWithValue(error.response?.data || { message: 'Unknown error' });
    }
  }
);

const reservationSlice = createSlice({
  name: 'reservation',
  initialState: {
    reservations: [] as Reservation[],
    searchResults: null as RoomSearchResponse | null,
    loading: false,
    error: null as string | null,
  },
  reducers: {
    clearError: (state) => {
      state.error = null;
    },
    clearSearchResults: (state) => {
      state.searchResults = null;
    },
  },
  extraReducers: (builder) => {
    builder
      .addCase(createReservation.pending, (state) => {
        state.loading = true;
        state.error = null;
      })
      .addCase(createReservation.fulfilled, (state, action) => {
        state.loading = false;
        state.reservations.push(action.payload.reservation);
      })
      .addCase(createReservation.rejected, (state, action) => {
        state.loading = false;
        state.error = action.payload as string;
      });
  },
});

export default reservationSlice.reducer;
```

**完了基準**:
- [ ] Redux Toolkit状態管理実装完了
- [ ] React Query キャッシュ戦略実装
- [ ] エラー状態管理実装完了

---

### Day 19-20: 最終統合・性能最適化

#### タスク 4.4: E2E テスト + 性能最適化（12h）

**Cypress E2Eテスト**:
```typescript
// cypress/e2e/reservation.cy.ts
describe('会議室予約システム', () => {
  beforeEach(() => {
    cy.login('member@example.com', 'password');
  });

  it('会議室予約の完全なフロー', () => {
    // 1. 会議室検索
    cy.visit('/reservations/new');
    cy.get('[data-testid=reservation-date]').type('2024-02-15');
    cy.get('[data-testid=start-time]').select('10:00');
    cy.get('[data-testid=end-time]').select('12:00');
    cy.get('[data-testid=search-button]').click();

    // 2. 検索結果確認
    cy.get('[data-testid=room-list]').should('be.visible');
    cy.get('[data-testid=room-card]').first().click();

    // 3. 予約詳細入力
    cy.get('[data-testid=purpose]').type('週次定例会議');
    cy.get('[data-testid=attendee-count]').clear().type('6');
    cy.get('[data-testid=notes]').type('プロジェクター使用予定');

    // 4. 予約確定
    cy.get('[data-testid=confirm-button]').click();
    cy.get('[data-testid=success-message]').should('contain', '予約が確定しました');

    // 5. 予約一覧で確認
    cy.visit('/reservations');
    cy.get('[data-testid=reservation-list]').should('contain', '週次定例会議');
  });

  it('権限エラーのハンドリング', () => {
    cy.logout();
    cy.visit('/reservations/new');
    cy.url().should('include', '/login');
  });
});
```

**性能最適化実装**:
```java
// キャッシュ戦略実装
@Configuration
@EnableCaching
public class CacheConfig {
    
    @Bean
    public CacheManager cacheManager() {
        RedisCacheManager.Builder builder = RedisCacheManager
            .RedisCacheManagerBuilder
            .fromConnectionFactory(redisConnectionFactory())
            .cacheDefaults(cacheConfiguration());
            
        return builder.build();
    }
    
    private RedisCacheConfiguration cacheConfiguration() {
        return RedisCacheConfiguration.defaultCacheConfig()
            .entryTtl(Duration.ofMinutes(10))
            .serializeKeysWith(RedisSerializationContext.SerializationPair
                .fromSerializer(new StringRedisSerializer()))
            .serializeValuesWith(RedisSerializationContext.SerializationPair
                .fromSerializer(new GenericJackson2JsonRedisSerializer()));
    }
}

// 非同期処理実装
@Service
public class NotificationService {
    
    @Async
    public void sendReservationConfirmation(Reservation reservation) {
        // メール送信処理（非同期）
        emailService.sendConfirmationEmail(reservation);
        
        // 監査ログ記録
        auditLogService.logNotificationSent(reservation);
    }
}
```

**完了基準**:
- [ ] E2E テストスイート完成
- [ ] 性能基準達成（レスポンス時間 < 500ms）
- [ ] キャッシュ戦略実装完了
- [ ] 非同期処理実装完了

---

## 品質チェックポイント

### 継続的品質管理

#### コミット毎の自動チェック
```yaml
# .github/workflows/ci.yml
name: Continuous Integration
on:
  push:
    branches: [ main, develop ]
  pull_request:
    branches: [ main ]

jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - name: Set up JDK 21
        uses: actions/setup-java@v3
        with:
          java-version: '21'
          distribution: 'temurin'
      
      - name: Cache Gradle packages
        uses: actions/cache@v3
        with:
          path: |
            ~/.gradle/caches
            ~/.gradle/wrapper
          key: ${{ runner.os }}-gradle-${{ hashFiles('**/*.gradle*') }}
          restore-keys: |
            ${{ runner.os }}-gradle-
      
      - name: Run tests
        run: ./gradlew test
      
      - name: Run PMD
        run: ./gradlew pmdMain pmdTest
      
      - name: Run Checkstyle  
        run: ./gradlew checkstyleMain checkstyleTest
      
      - name: Run SpotBugs
        run: ./gradlew spotbugsMain spotbugsTest
      
      - name: Generate test report
        run: ./gradlew jacocoTestReport
      
      - name: Check coverage
        run: ./gradlew jacocoTestCoverageVerification
```

#### 週次品質レビュー
- [ ] **コードカバレッジ**: 90%以上維持
- [ ] **静的解析**: PMD、Checkstyle、SpotBugs全クリア
- [ ] **セキュリティ**: OWASP依存関係チェック パス
- [ ] **性能**: レスポンス時間 95%ile < 500ms
- [ ] **可用性**: アップタイム 99.5%以上

### デプロイメント品質基準

#### ステージング環境検証
```bash
#!/bin/bash
# deploy-staging.sh

echo "=== ステージング環境デプロイメント開始 ==="

# 1. アプリケーションビルド
./gradlew build -x test

# 2. Docker イメージビルド
docker build -t mrs-app:staging .

# 3. ステージング環境デプロイ
docker-compose -f docker-compose.staging.yml up -d

# 4. ヘルスチェック
echo "ヘルスチェック実行中..."
timeout 300 bash -c 'until curl -f http://localhost:8080/actuator/health; do sleep 5; done'

# 5. スモークテスト
echo "スモークテスト実行中..."
curl -f http://localhost:8080/api/health || exit 1

# 6. E2Eテスト実行
npx cypress run --env baseUrl=http://localhost:8080

echo "=== ステージング環境デプロイメント完了 ==="
```

#### 本番デプロイ前チェック
- [ ] **全テストパス**: Unit + Integration + E2E
- [ ] **セキュリティスキャン**: 脆弱性ゼロ
- [ ] **性能テスト**: 負荷テスト クリア
- [ ] **バックアップ**: データベースバックアップ完了
- [ ] **ロールバック計画**: 緊急時手順確認済み

## リスク対策・緊急時対応

### 技術リスク対策

#### 同時アクセス制御
```java
// 悲観的ロック + リトライ機構
@Service
public class ReservationConcurrencyService {
    
    @Retryable(value = {OptimisticLockingFailureException.class}, maxAttempts = 3)
    @Transactional
    public Reservation createReservationWithLock(ReservationRequest request) {
        // 悲観的ロックで排他制御
        MeetingRoom room = meetingRoomRepository.findByIdForUpdate(request.getRoomId())
            .orElseThrow(() -> new MeetingRoomNotFoundException());
            
        // 重複チェック（ダブルチェック）
        if (!reservationDomainService.isRoomAvailable(room.getId(), request.getPeriod())) {
            throw new RoomNotAvailableException("指定時間は既に予約済みです");
        }
        
        return reservationRepository.save(createReservation(request));
    }
}
```

#### データ整合性保証
```sql
-- データベース制約による整合性保証
ALTER TABLE reservations 
ADD CONSTRAINT check_time_order 
CHECK (start_time < end_time);

ALTER TABLE reservations 
ADD CONSTRAINT check_attendee_capacity 
CHECK (attendee_count <= (SELECT capacity FROM meeting_rooms WHERE id = meeting_room_id));

-- 重複予約防止ユニーク制約
CREATE UNIQUE INDEX idx_no_overlapping_reservations 
ON reservations (meeting_room_id, reservation_date, int4range(extract(hour from start_time) * 60 + extract(minute from start_time), extract(hour from end_time) * 60 + extract(minute from end_time), '[)'))
WHERE status IN ('CONFIRMED', 'PENDING');
```

### 運用リスク対策

#### 監視・アラート設定
```yaml
# prometheus/alert-rules.yml
groups:
- name: mrs-application
  rules:
  - alert: HighErrorRate
    expr: rate(http_requests_total{status=~"5.."}[5m]) > 0.05
    for: 2m
    labels:
      severity: critical
    annotations:
      summary: "アプリケーションエラー率が異常に高い"
      
  - alert: DatabaseConnectionFailure  
    expr: up{job="postgresql"} == 0
    for: 1m
    labels:
      severity: critical
    annotations:
      summary: "データベース接続失敗"
      
  - alert: ReservationCreationFailure
    expr: increase(reservation_creation_failures_total[10m]) > 10
    for: 5m
    labels:
      severity: warning
    annotations:
      summary: "予約作成エラーが頻発"
```

#### 緊急時対応手順
```bash
#!/bin/bash
# emergency-response.sh

case "$1" in
  "database-down")
    echo "データベース障害対応開始"
    # 1. レプリカへの自動フェイルオーバー
    kubectl patch service postgres-service -p '{"spec":{"selector":{"app":"postgres-replica"}}}'
    # 2. アプリケーション再起動
    kubectl rollout restart deployment/mrs-backend
    ;;
    
  "high-load")
    echo "高負荷対応開始" 
    # 1. オートスケーリング強制発動
    kubectl scale deployment mrs-backend --replicas=6
    # 2. 非重要API一時停止
    kubectl patch ingress mrs-ingress --type='json' -p='[{"op":"add","path":"/spec/rules/0/http/paths/-","value":{"path":"/api/reports","pathType":"Prefix","backend":{"service":{"name":"maintenance-service","port":{"number":80}}}}}]'
    ;;
    
  "security-breach")
    echo "セキュリティ インシデント対応開始"
    # 1. 全JWT トークン無効化
    kubectl create job token-revoke --from=cronjob/token-cleanup
    # 2. 緊急メンテナンスモード
    kubectl patch ingress mrs-ingress --type='json' -p='[{"op":"replace","path":"/spec/rules/0/http/paths/0/backend/service/name","value":"maintenance-service"}]'
    ;;
esac
```

## 完了条件・検収基準

### 機能完成基準
- [ ] **全機能動作確認**: 要件定義の100%実装完了
- [ ] **セキュリティ基準**: OWASP Top 10完全対応
- [ ] **性能基準**: 全性能要件クリア
- [ ] **可用性基準**: 99.5%稼働率達成見込み

### 品質基準
- [ ] **テストカバレッジ**: 90%以上（Unit + Integration + E2E）
- [ ] **静的解析**: 重要度：高 の問題ゼロ
- [ ] **セキュリティスキャン**: 重大・高危険度脆弱性ゼロ
- [ ] **アクセシビリティ**: WCAG 2.1 AA準拠

### ドキュメント完成基準
- [ ] **API仕様書**: OpenAPI 3.0完全準拠
- [ ] **運用マニュアル**: デプロイ・監視・障害対応手順書
- [ ] **ユーザーマニュアル**: エンドユーザー向け操作手順書
- [ ] **セキュリティレポート**: 脆弱性対策実装証跡

---

## 実装完了後の継続改善

### Phase 5: 運用最適化（Optional）
- パフォーマンスチューニング
- ユーザーフィードバック対応
- 機能拡張（モバイルアプリ、レポート機能）
- 多言語対応

### メンテナンス計画
- 月次セキュリティパッチ適用
- 四半期機能リリース
- 年次アーキテクチャレビュー

**4週間での本番レディ会議室予約システム実装完了予定**