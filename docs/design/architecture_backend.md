---
title: バックエンドアーキテクチャ設計 - React Job Board Application
description: 求人掲載・管理システムのバックエンドアーキテクチャ設計書
published: true
date: 2025-09-11T07:00:00.000Z
tags: backend-architecture, domain-model, layered-architecture, job-board
editor: markdown
dateCreated: 2025-09-11T07:00:00.000Z
---

# バックエンドアーキテクチャ設計 - React Job Board Application

## アーキテクチャパターン選択

### 業務特性分析

#### 業務領域カテゴリー分析

| 機能領域 | カテゴリー | 理由 |
|----------|------------|------|
| **求人管理** | 中核の業務領域 | 組織の採用活動の中心機能、競争優位性を決定 |
| **応募管理** | 中核の業務領域 | 選考プロセスの透明性・効率性が組織価値に直結 |
| **組織管理** | 中核の業務領域 | 権限管理と組織運営の基盤機能 |
| **通知・レポート** | 補完・一般との連携 | 中核業務を支援する補助的機能 |

#### データ構造複雑性評価

```plantuml
@startuml
title データ構造複雑性分析

entity "User" {
  + id: UUID
  + email: String
  + role: UserRole
  --
  権限制御の複雑性
}

entity "Organization" {
  + id: UUID
  + name: String
  + settings: JSON
  --
  階層管理の複雑性
}

entity "Job" {
  + id: UUID
  + position: String
  + status: JobStatus
  + organizationId: UUID
  --
  状態遷移の複雑性
}

entity "Application" {
  + id: UUID
  + jobId: UUID
  + userId: UUID
  + status: ApplicationStatus
  --
  選考プロセスの複雑性
}

User ||--o{ Application
Job ||--o{ Application
Organization ||--o{ Job
Organization ||--o{ User

note top of User
  ・3つのロール（求職者・採用担当者・管理者）
  ・組織横断の権限管理
  ・動的な権限変更
end note

note top of Job
  ・下書き→公開→一時停止→終了の状態遷移
  ・組織内権限による操作制限
  ・検索インデックスとの同期
end note

note top of Application
  ・8段階の選考状況遷移
  ・監査ログの必要性
  ・並行処理での整合性確保
end note

@enduml
```

**複雑性判定**: **複雑**
- エンティティ間の多様な関係性
- 複数の状態遷移管理
- 権限ベースアクセス制御
- 監査ログ要件

#### 特殊要件評価

| 要件 | 適用 | 詳細 |
|------|------|------|
| **金額を扱う** | ❌ | 給与情報は表示のみ、決済機能なし |
| **分析** | ⭕ | 求人・応募データの集計・分析 |
| **監査記録が必要** | ⭕ | 応募履歴・選考プロセスの完全な記録 |

### 選択されたアーキテクチャパターン

#### 主要パターン: **ドメインモデルパターン**

```plantuml
@startuml
title アーキテクチャパターン選択フロー - React Job Board Application

start

if (業務領域のカテゴリー) then (中核の業務領域)
  if (金額を扱う/分析/監査記録が必要か?) then (はい - 監査記録必要)
    :イベント履歴式ドメインモデル;
    :ピラミッド形のテスト;
    :CQRS;
    note right
      ただし、MVP段階では
      シンプルなドメインモデル
      から開始
    end note
  else (いいえ)
    :ドメインモデル;
    :ピラミッド形のテスト;
    if (永続化モデルは複数か?) then (いいえ)
      :ポートとアダプター;
    else (はい)
      :CQRS;
    endif
  endif
else (補完、一般との連携)
  if (データ構造が複雑か?) then (いいえ)
    :トランザクションスクリプト;
    :逆ピラミッド形のテスト;
  else (はい)
    :アクティブレコード;
    :ダイヤモンド形のテスト;
  endif
endif

stop

@enduml
```

**MVP フェーズの判断**:
監査記録が必要だが、初期段階では**ドメインモデルパターン**から開始し、将来的に**イベント履歴式ドメインモデル**への移行を計画する。

#### アーキテクチャスタイル: **レイヤードアーキテクチャ（4層）**

永続化モデルが複数（RDBMS + Redis + File Storage）のため、4層構成を採用。

## アーキテクチャ設計詳細

### 1. レイヤー構成

```plantuml
@startuml
title 4層レイヤードアーキテクチャ

package "プレゼンテーション層" {
  [REST Controller] as controller
  [GraphQL Resolver] as graphql
  [WebSocket Handler] as websocket
  [Middleware] as middleware
}

package "アプリケーション層" {
  [Application Service] as appservice
  [Use Case] as usecase
  [Command Handler] as command
  [Query Handler] as query
}

package "ドメイン層" {
  [Domain Service] as domainservice
  [Entity] as entity
  [Value Object] as vo
  [Domain Event] as event
  [Repository Interface] as repointerface
}

package "インフラストラクチャ層" {
  [Repository Implementation] as repoimpl
  [External API Client] as apiclient
  [File Storage] as filestorage
  [Message Queue] as mq
  [Database] as db
  [Cache] as cache
}

controller --> appservice
graphql --> appservice
websocket --> appservice
middleware --> appservice

appservice --> usecase
appservice --> command
appservice --> query

usecase --> domainservice
usecase --> entity
command --> entity
query --> repointerface

domainservice --> entity
domainservice --> vo
entity --> event
entity --> vo

repointerface <|-- repoimpl
repoimpl --> db
repoimpl --> cache
apiclient --> [External System]
filestorage --> [File System]
mq --> [Message Broker]

@enduml
```

### 2. レイヤー責務詳細

#### プレゼンテーション層
```java
// HTTP要求/応答の処理、認証・認可、入力検証
@RestController
@RequestMapping("/api/jobs")
public class JobController {
    private final CreateJobUseCase createJobUseCase;
    private final FindJobsQuery findJobsQuery;
    
    @PostMapping
    @PreAuthorize("hasRole('RECRUITER')")
    public ResponseEntity<JobResponse> createJob(
        @Valid @RequestBody CreateJobRequest request,
        Authentication auth
    ) {
        var command = CreateJobCommand.builder()
            .organizationId(extractOrganizationId(auth))
            .position(request.getPosition())
            .department(request.getDepartment())
            .location(request.getLocation())
            .info(request.getInfo())
            .build();
            
        var jobId = createJobUseCase.execute(command);
        var response = JobResponse.from(jobId);
        
        return ResponseEntity.status(HttpStatus.CREATED).body(response);
    }
    
    @GetMapping
    public ResponseEntity<JobListResponse> findJobs(
        @RequestParam(required = false) String position,
        @RequestParam(required = false) String location,
        @RequestParam(defaultValue = "0") int page,
        @RequestParam(defaultValue = "20") int size
    ) {
        var query = FindJobsQuery.builder()
            .position(position)
            .location(location)
            .page(page)
            .size(size)
            .build();
            
        var result = findJobsQuery.execute(query);
        return ResponseEntity.ok(JobListResponse.from(result));
    }
}
```

#### アプリケーション層
```java
// ユースケース制御、トランザクション境界の管理
@Service
@Transactional
public class CreateJobUseCase {
    private final JobRepository jobRepository;
    private final OrganizationRepository organizationRepository;
    private final JobDomainService jobDomainService;
    
    public JobId execute(CreateJobCommand command) {
        // 1. 組織の存在確認
        var organization = organizationRepository.findById(command.getOrganizationId())
            .orElseThrow(() -> new OrganizationNotFoundException(command.getOrganizationId()));
            
        // 2. ドメインサービスによる求人作成
        var job = jobDomainService.createJob(
            command.getOrganizationId(),
            command.getPosition(),
            command.getDepartment(),
            command.getLocation(),
            command.getInfo()
        );
        
        // 3. 永続化
        jobRepository.save(job);
        
        // 4. ドメインイベントの発行
        job.getUncommittedEvents().forEach(this::publishEvent);
        
        return job.getId();
    }
    
    private void publishEvent(DomainEvent event) {
        // イベント発行処理
    }
}
```

#### ドメイン層
```java
// ビジネスルール、不変条件、ドメインサービス
@Entity
public class Job {
    @Id
    private JobId id;
    private OrganizationId organizationId;
    private String position;
    private String department;
    private String location;
    private JobInfo info;
    private JobStatus status;
    private LocalDateTime createdAt;
    private LocalDateTime updatedAt;
    
    // ファクトリーメソッド
    public static Job create(
        JobId id,
        OrganizationId organizationId,
        String position,
        String department,
        String location,
        JobInfo info
    ) {
        validatePosition(position);
        validateDepartment(department);
        validateLocation(location);
        
        var job = new Job();
        job.id = id;
        job.organizationId = organizationId;
        job.position = position;
        job.department = department;
        job.location = location;
        job.info = info;
        job.status = JobStatus.DRAFT;
        job.createdAt = LocalDateTime.now();
        job.updatedAt = LocalDateTime.now();
        
        // ドメインイベント追加
        job.addEvent(new JobCreatedEvent(id, organizationId, position));
        
        return job;
    }
    
    // ビジネスルール
    public void publish() {
        if (status != JobStatus.DRAFT) {
            throw new IllegalJobStatusTransitionException(
                "下書き状態の求人のみ公開できます"
            );
        }
        
        if (!isValidForPublish()) {
            throw new InvalidJobForPublishException(
                "公開に必要な情報が不足しています"
            );
        }
        
        this.status = JobStatus.PUBLISHED;
        this.updatedAt = LocalDateTime.now();
        addEvent(new JobPublishedEvent(this.id));
    }
    
    public void pause() {
        if (status != JobStatus.PUBLISHED) {
            throw new IllegalJobStatusTransitionException(
                "公開中の求人のみ一時停止できます"
            );
        }
        
        this.status = JobStatus.PAUSED;
        this.updatedAt = LocalDateTime.now();
        addEvent(new JobPausedEvent(this.id));
    }
    
    private boolean isValidForPublish() {
        return StringUtils.hasText(position) &&
               StringUtils.hasText(department) &&
               StringUtils.hasText(location) &&
               info != null && info.isValid();
    }
    
    private static void validatePosition(String position) {
        if (!StringUtils.hasText(position)) {
            throw new IllegalArgumentException("職種名は必須です");
        }
        if (position.length() > 100) {
            throw new IllegalArgumentException("職種名は100文字以内で入力してください");
        }
    }
}

// ドメインサービス
@Service
public class JobDomainService {
    public Job createJob(
        OrganizationId organizationId,
        String position,
        String department,
        String location,
        JobInfo info
    ) {
        var jobId = JobId.generate();
        return Job.create(jobId, organizationId, position, department, location, info);
    }
    
    public boolean canUserEditJob(UserId userId, Job job) {
        // 編集権限のビジネスルール
        // - 組織内のユーザーのみ編集可能
        // - 採用担当者以上の権限が必要
        // - 求人が削除されていない
        return true; // 実装省略
    }
}
```

#### インフラストラクチャ層
```java
// 外部システム連携、永続化、技術的関心事
@Repository
public class JpaJobRepository implements JobRepository {
    private final SpringDataJobRepository jpaRepository;
    private final JobMapper mapper;
    
    @Override
    public Optional<Job> findById(JobId jobId) {
        return jpaRepository.findById(jobId.getValue())
            .map(mapper::toDomain);
    }
    
    @Override
    public List<Job> findByOrganizationId(OrganizationId organizationId) {
        return jpaRepository.findByOrganizationId(organizationId.getValue())
            .stream()
            .map(mapper::toDomain)
            .collect(Collectors.toList());
    }
    
    @Override
    public void save(Job job) {
        var entity = mapper.toEntity(job);
        jpaRepository.save(entity);
    }
    
    @Override
    public Page<Job> findBySearchCriteria(JobSearchCriteria criteria, Pageable pageable) {
        var specification = JobSpecifications.fromCriteria(criteria);
        var entityPage = jpaRepository.findAll(specification, pageable);
        
        return entityPage.map(mapper::toDomain);
    }
}

// 外部API連携
@Component
public class EmailNotificationService implements NotificationService {
    private final EmailServiceClient emailClient;
    
    @Override
    public void sendJobApplicationConfirmation(UserId userId, JobId jobId) {
        var template = EmailTemplate.JOB_APPLICATION_CONFIRMATION;
        var context = createEmailContext(userId, jobId);
        
        emailClient.sendEmail(template, context);
    }
}
```

### 3. ドメインモデル設計

#### 主要エンティティ

```plantuml
@startuml
title ドメインモデル

package "User Aggregate" {
  entity User {
    + id: UserId
    + email: EmailAddress
    + profile: UserProfile
    + role: UserRole
    + organizationId: OrganizationId?
    --
    + changeRole(UserRole): void
    + joinOrganization(OrganizationId): void
    + leaveOrganization(): void
  }
  
  value UserProfile {
    + firstName: String
    + lastName: String
    + phoneNumber: PhoneNumber
    + resumeFile: FileReference?
  }
  
  enum UserRole {
    JOB_SEEKER
    RECRUITER
    ORGANIZATION_ADMIN
  }
}

package "Organization Aggregate" {
  entity Organization {
    + id: OrganizationId
    + name: String
    + info: OrganizationInfo
    + settings: OrganizationSettings
    --
    + updateInfo(OrganizationInfo): void
    + inviteUser(EmailAddress, UserRole): void
    + removeUser(UserId): void
  }
  
  value OrganizationInfo {
    + industry: String
    + size: CompanySize
    + location: Address
    + description: String
    + logoFile: FileReference?
  }
}

package "Job Aggregate" {
  entity Job {
    + id: JobId
    + organizationId: OrganizationId
    + position: String
    + department: String
    + location: String
    + info: JobInfo
    + status: JobStatus
    + publishedAt: LocalDateTime?
    --
    + publish(): void
    + pause(): void
    + close(): void
    + canBeEditedBy(UserId): boolean
  }
  
  value JobInfo {
    + description: String
    + requirements: List<String>
    + benefits: List<String>
    + employmentType: EmploymentType
    + salaryRange: SalaryRange?
  }
  
  enum JobStatus {
    DRAFT
    PUBLISHED
    PAUSED
    CLOSED
  }
}

package "Application Aggregate" {
  entity Application {
    + id: ApplicationId
    + jobId: JobId
    + userId: UserId
    + applicationData: ApplicationData
    + status: ApplicationStatus
    + submittedAt: LocalDateTime
    + statusHistory: List<StatusChange>
    --
    + updateStatus(ApplicationStatus, String): void
    + addNote(String): void
    + canBeViewedBy(UserId): boolean
  }
  
  value ApplicationData {
    + coverLetter: String
    + resumeFile: FileReference
    + additionalInfo: String?
  }
  
  enum ApplicationStatus {
    SUBMITTED
    SCREENING
    INTERVIEW_SCHEDULED
    INTERVIEW_COMPLETED
    FINAL_REVIEW
    ACCEPTED
    REJECTED
    WITHDRAWN
  }
}

User ||--o{ Application
Job ||--o{ Application
Organization ||--o{ Job
Organization ||--o{ User

@enduml
```

#### 状態遷移管理

```plantuml
@startuml
title 求人ステータス遷移

[*] --> DRAFT : 求人作成

state DRAFT {
  DRAFT : 編集可能
  DRAFT : 公開前状態
}

DRAFT --> PUBLISHED : publish()
DRAFT --> [*] : delete()

state PUBLISHED {
  PUBLISHED : 応募受付中
  PUBLISHED : 検索可能
}

PUBLISHED --> PAUSED : pause()
PUBLISHED --> CLOSED : close()

state PAUSED {
  PAUSED : 一時停止
  PAUSED : 検索対象外
}

PAUSED --> PUBLISHED : resume()
PAUSED --> CLOSED : close()

state CLOSED {
  CLOSED : 募集終了
  CLOSED : 応募不可
}

CLOSED --> [*]

@enduml
```

```plantuml
@startuml
title 応募ステータス遷移

[*] --> SUBMITTED : 応募申請

SUBMITTED --> SCREENING : 書類選考開始
SUBMITTED --> WITHDRAWN : 応募者辞退

SCREENING --> INTERVIEW_SCHEDULED : 書類選考通過
SCREENING --> REJECTED : 書類選考不通過
SCREENING --> WITHDRAWN : 応募者辞退

INTERVIEW_SCHEDULED --> INTERVIEW_COMPLETED : 面接実施
INTERVIEW_SCHEDULED --> WITHDRAWN : 応募者辞退

INTERVIEW_COMPLETED --> FINAL_REVIEW : 最終選考へ
INTERVIEW_COMPLETED --> REJECTED : 面接不通過

FINAL_REVIEW --> ACCEPTED : 内定
FINAL_REVIEW --> REJECTED : 最終選考不通過

ACCEPTED --> [*]
REJECTED --> [*]
WITHDRAWN --> [*]

@enduml
```

### 4. データアクセス戦略

#### 永続化モデルマッピング

| ドメインオブジェクト | 永続化方式 | 理由 |
|-------------------|------------|------|
| **User Aggregate** | RDBMS (PostgreSQL) | ACID特性、複雑なクエリ |
| **Organization Aggregate** | RDBMS (PostgreSQL) | 関係性管理、整合性保証 |
| **Job Aggregate** | RDBMS + Search Index | 永続化 + 高速検索 |
| **Application Aggregate** | RDBMS + File Storage | 構造化データ + ファイル |
| **Session Data** | Redis | 高速アクセス、TTL管理 |
| **Search Index** | Elasticsearch/Solr | 全文検索、ファジー検索 |

#### Repository実装パターン

```java
// 基底インターフェース
public interface Repository<T, ID> {
    Optional<T> findById(ID id);
    void save(T entity);
    void delete(T entity);
}

// ドメイン固有リポジトリ
public interface JobRepository extends Repository<Job, JobId> {
    List<Job> findByOrganizationId(OrganizationId organizationId);
    Page<Job> findBySearchCriteria(JobSearchCriteria criteria, Pageable pageable);
    List<Job> findPublishedJobsByLocation(String location);
    Optional<Job> findByIdAndOrganizationId(JobId jobId, OrganizationId organizationId);
}

// 検索特化リポジトリ
public interface JobSearchRepository {
    SearchResult<JobSummary> search(JobSearchQuery query);
    List<String> suggestPositions(String partial);
    Map<String, Long> aggregateByLocation();
}
```

### 5. 非機能要件対応

#### パフォーマンス対応

```java
// キャッシュ戦略
@Service
public class JobQueryService {
    private final JobRepository jobRepository;
    private final RedisTemplate<String, Object> redisTemplate;
    
    @Cacheable(value = "popular-jobs", key = "#location")
    public List<JobSummary> findPopularJobs(String location) {
        return jobRepository.findPopularJobsByLocation(location);
    }
    
    @CacheEvict(value = "popular-jobs", allEntries = true)
    public void clearJobCache() {
        // キャッシュクリア
    }
}

// データベース最適化
@Entity
@Table(name = "jobs", indexes = {
    @Index(name = "idx_organization_status", columnList = "organization_id, status"),
    @Index(name = "idx_location_position", columnList = "location, position"),
    @Index(name = "idx_published_at", columnList = "published_at")
})
public class JobEntity {
    // エンティティ定義
}
```

#### セキュリティ対応

```java
// 認証・認可
@Configuration
@EnableGlobalMethodSecurity(prePostEnabled = true)
public class SecurityConfig {
    
    @Bean
    public MethodSecurityExpressionHandler methodSecurityExpressionHandler() {
        return new JobBoardMethodSecurityExpressionHandler();
    }
}

// カスタム権限チェック
@Component
public class JobBoardMethodSecurityExpressionHandler extends DefaultMethodSecurityExpressionHandler {
    
    @PreAuthorize("@jobSecurityService.canEditJob(authentication.name, #jobId)")
    public void updateJob(JobId jobId, UpdateJobCommand command) {
        // 実装
    }
}

@Service
public class JobSecurityService {
    public boolean canEditJob(String username, JobId jobId) {
        // 編集権限チェックロジック
        return true;
    }
}
```

### 6. API設計

#### REST API エンドポイント

```yaml
# OpenAPI 3.0 仕様
paths:
  /api/jobs:
    get:
      summary: 求人検索
      parameters:
        - name: position
          in: query
          schema:
            type: string
        - name: location
          in: query
          schema:
            type: string
        - name: page
          in: query
          schema:
            type: integer
            default: 0
        - name: size
          in: query
          schema:
            type: integer
            default: 20
      responses:
        '200':
          description: 検索結果
          content:
            application/json:
              schema:
                $ref: '#/components/schemas/JobListResponse'
    
    post:
      summary: 求人作成
      security:
        - bearerAuth: []
      requestBody:
        required: true
        content:
          application/json:
            schema:
              $ref: '#/components/schemas/CreateJobRequest'
      responses:
        '201':
          description: 作成成功
          content:
            application/json:
              schema:
                $ref: '#/components/schemas/JobResponse'
        '403':
          description: 権限なし

  /api/jobs/{jobId}:
    get:
      summary: 求人詳細取得
      parameters:
        - name: jobId
          in: path
          required: true
          schema:
            type: string
            format: uuid
      responses:
        '200':
          description: 求人詳細
          content:
            application/json:
              schema:
                $ref: '#/components/schemas/JobDetailResponse'
        '404':
          description: 求人が見つからない

    put:
      summary: 求人更新
      security:
        - bearerAuth: []
      parameters:
        - name: jobId
          in: path
          required: true
          schema:
            type: string
            format: uuid
      requestBody:
        required: true
        content:
          application/json:
            schema:
              $ref: '#/components/schemas/UpdateJobRequest'
      responses:
        '200':
          description: 更新成功
        '403':
          description: 権限なし
        '404':
          description: 求人が見つからない

  /api/jobs/{jobId}/applications:
    get:
      summary: 応募一覧取得
      security:
        - bearerAuth: []
      parameters:
        - name: jobId
          in: path
          required: true
          schema:
            type: string
            format: uuid
      responses:
        '200':
          description: 応募一覧
          content:
            application/json:
              schema:
                $ref: '#/components/schemas/ApplicationListResponse'

    post:
      summary: 求人応募
      security:
        - bearerAuth: []
      parameters:
        - name: jobId
          in: path
          required: true
          schema:
            type: string
            format: uuid
      requestBody:
        required: true
        content:
          multipart/form-data:
            schema:
              $ref: '#/components/schemas/CreateApplicationRequest'
      responses:
        '201':
          description: 応募成功
        '409':
          description: 重複応募
```

### 7. テスト戦略

#### ピラミッド形テスト構成

```plantuml
@startuml
title ドメインモデルパターンのテスト戦略

rectangle "E2Eテスト (10%)" as e2e #lightcoral {
  [API統合テスト]
  [シナリオテスト]
}

rectangle "統合テスト (20%)" as integration #orange {
  [Repository テスト]
  [外部API テスト]
  [データベース テスト]
}

rectangle "ユニットテスト (70%)" as unit #lightgreen {
  [ドメインエンティティ テスト]
  [ドメインサービス テスト]
  [ユースケース テスト]
  [バリューオブジェクト テスト]
}

unit -up-> integration
integration -up-> e2e

@enduml
```

#### テスト実装例

```java
// ドメインエンティティテスト
class JobTest {
    
    @Test
    void 求人を公開状態に変更できる() {
        // Given
        var job = Job.create(
            JobId.generate(),
            OrganizationId.of("org-1"),
            "Software Engineer",
            "Engineering",
            "Tokyo",
            JobInfo.builder()
                .description("Java developer position")
                .requirements(List.of("Java", "Spring Boot"))
                .build()
        );
        
        // When
        job.publish();
        
        // Then
        assertThat(job.getStatus()).isEqualTo(JobStatus.PUBLISHED);
        assertThat(job.getPublishedAt()).isNotNull();
    }
    
    @Test
    void 下書き状態以外の求人は公開できない() {
        // Given
        var job = createPublishedJob();
        
        // When & Then
        assertThatThrownBy(job::publish)
            .isInstanceOf(IllegalJobStatusTransitionException.class)
            .hasMessage("下書き状態の求人のみ公開できます");
    }
}

// ユースケーステスト
class CreateJobUseCaseTest {
    
    @Mock
    private JobRepository jobRepository;
    
    @Mock
    private OrganizationRepository organizationRepository;
    
    @InjectMocks
    private CreateJobUseCase useCase;
    
    @Test
    void 有効な組織IDで求人を作成できる() {
        // Given
        var organizationId = OrganizationId.of("org-1");
        var organization = Organization.create(organizationId, "Test Company");
        
        when(organizationRepository.findById(organizationId))
            .thenReturn(Optional.of(organization));
        
        var command = CreateJobCommand.builder()
            .organizationId(organizationId)
            .position("Software Engineer")
            .department("Engineering")
            .location("Tokyo")
            .info(createValidJobInfo())
            .build();
        
        // When
        var jobId = useCase.execute(command);
        
        // Then
        assertThat(jobId).isNotNull();
        verify(jobRepository).save(any(Job.class));
    }
}

// 統合テスト
@DataJpaTest
class JpaJobRepositoryTest {
    
    @Autowired
    private TestEntityManager entityManager;
    
    @Autowired
    private SpringDataJobRepository springDataRepository;
    
    private JpaJobRepository repository;
    
    @BeforeEach
    void setUp() {
        var mapper = new JobMapper();
        repository = new JpaJobRepository(springDataRepository, mapper);
    }
    
    @Test
    void 組織IDで求人を検索できる() {
        // Given
        var organizationId = OrganizationId.of("org-1");
        var job1 = createJobEntity(organizationId, "Engineer");
        var job2 = createJobEntity(organizationId, "Designer");
        var job3 = createJobEntity(OrganizationId.of("org-2"), "Manager");
        
        entityManager.persistAndFlush(job1);
        entityManager.persistAndFlush(job2);
        entityManager.persistAndFlush(job3);
        
        // When
        var jobs = repository.findByOrganizationId(organizationId);
        
        // Then
        assertThat(jobs)
            .hasSize(2)
            .extracting(Job::getPosition)
            .containsExactlyInAnyOrder("Engineer", "Designer");
    }
}
```

### 8. 運用監視

#### ログ戦略

```java
// 構造化ログ
@Slf4j
@Component
public class JobEventLogger {
    
    public void logJobCreated(JobId jobId, OrganizationId organizationId) {
        log.info("Job created: jobId={}, organizationId={}", 
            jobId.getValue(), organizationId.getValue());
    }
    
    public void logJobPublished(JobId jobId) {
        log.info("Job published: jobId={}", jobId.getValue());
    }
    
    public void logApplicationSubmitted(ApplicationId applicationId, JobId jobId, UserId userId) {
        log.info("Application submitted: applicationId={}, jobId={}, userId={}", 
            applicationId.getValue(), jobId.getValue(), userId.getValue());
    }
}

// 監査ログ
@Entity
@Table(name = "audit_logs")
public class AuditLog {
    @Id
    private String id;
    private String entityType;
    private String entityId;
    private String action;
    private String userId;
    private LocalDateTime timestamp;
    private String oldValue;
    private String newValue;
}
```

#### メトリクス監視

```java
// カスタムメトリクス
@Component
public class JobMetrics {
    private final MeterRegistry meterRegistry;
    private final Counter jobCreatedCounter;
    private final Counter applicationSubmittedCounter;
    private final Timer jobSearchTimer;
    
    public JobMetrics(MeterRegistry meterRegistry) {
        this.meterRegistry = meterRegistry;
        this.jobCreatedCounter = Counter.builder("jobs.created")
            .description("Number of jobs created")
            .register(meterRegistry);
        this.applicationSubmittedCounter = Counter.builder("applications.submitted")
            .description("Number of applications submitted")
            .register(meterRegistry);
        this.jobSearchTimer = Timer.builder("jobs.search.duration")
            .description("Time taken to search jobs")
            .register(meterRegistry);
    }
    
    public void incrementJobCreated() {
        jobCreatedCounter.increment();
    }
    
    public void incrementApplicationSubmitted() {
        applicationSubmittedCounter.increment();
    }
    
    public Timer.Sample startJobSearchTimer() {
        return Timer.start(meterRegistry);
    }
}
```

## 移行戦略

### MVP → フルシステム移行計画

```plantuml
@startuml
title 段階的アーキテクチャ移行

start

:MVP フェーズ;
note right
  ・ドメインモデルパターン
  ・4層レイヤードアーキテクチャ
  ・PostgreSQL + Redis
  ・基本的な監査ログ
end note

:機能拡張フェーズ;
note right
  ・イベントドリブン要素追加
  ・検索エンジン統合
  ・高度な監査機能
  ・分析機能実装
end note

:スケールアップフェーズ;
note right
  ・イベント履歴式ドメインモデル
  ・CQRS パターン導入
  ・マイクロサービス分割検討
  ・高可用性対応
end note

stop

@enduml
```

### 技術的負債管理

1. **設計負債**
   - 定期的なアーキテクチャレビュー
   - リファクタリング計画の策定
   - 技術的意思決定の記録（ADR）

2. **パフォーマンス負債**
   - 継続的なパフォーマンス監視
   - ボトルネック特定と改善
   - キャッシュ戦略の最適化

3. **セキュリティ負債**
   - 定期的なセキュリティ監査
   - 脆弱性スキャンの自動化
   - セキュリティパッチの適用

## まとめ

React Job Board Application のバックエンドアーキテクチャは、以下の特徴を持つ設計としました：

### 採用アーキテクチャパターン
- **ドメインモデルパターン**: 中核業務領域の複雑なビジネスルールを適切に表現
- **4層レイヤードアーキテクチャ**: 複数の永続化モデルを統合管理
- **ピラミッド形テスト**: ドメインロジックの品質を重視

### 主要設計判断
1. **業務特性重視**: 求人・応募管理を中核領域として設計
2. **段階的成長**: MVP から本格システムへの移行を考慮
3. **品質確保**: 監査ログ・メトリクス・包括的テスト
4. **拡張性**: 将来のイベント履歴式への移行を視野

### 次のステップ
1. フロントエンドアーキテクチャ設計
2. API 仕様の詳細化
3. データベース設計
4. 技術スタック選定

このアーキテクチャにより、React Job Board Application は高品質で保守性の高いバックエンドシステムとして構築されます。