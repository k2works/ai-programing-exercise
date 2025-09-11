---
title: UI設計 - React Job Board Application
description: 求人掲載・管理システムのUI設計書
published: true
date: 2025-09-11T09:00:00.000Z
tags: ui-design, ux-design, ooux, react, job-board
editor: markdown
dateCreated: 2025-09-11T09:00:00.000Z
---

# UI設計 - React Job Board Application

## オブジェクト指向UI設計（OOUX）の適用

### 主要オブジェクトの特定

#### 1. 求人（Job）
- **属性**: 職種名、部門、勤務地、職務内容、必要スキル、給与、雇用形態、ステータス
- **アクション**: 
  - Create: 求人作成（採用担当者）
  - Read: 求人検索・詳細表示（全ユーザー）
  - Update: 求人編集（採用担当者）
  - Delete: 求人削除（採用担当者）
  - 特殊アクション: 公開/一時停止/終了

#### 2. 応募（Application）
- **属性**: 応募者情報、求人ID、志望動機、履歴書、応募日、選考状況
- **アクション**:
  - Create: 応募申請（求職者）
  - Read: 応募履歴確認（求職者）、応募者管理（採用担当者）
  - Update: 選考状況更新（採用担当者）
  - Delete: 応募取り下げ（求職者）

#### 3. ユーザー（User）
- **属性**: 名前、メールアドレス、ロール、組織ID、プロフィール情報
- **アクション**:
  - Create: ユーザー登録
  - Read: プロフィール表示
  - Update: プロフィール編集
  - Delete: アカウント削除

#### 4. 組織（Organization）
- **属性**: 組織名、業界、規模、所在地、企業情報
- **アクション**:
  - Read: 組織情報表示
  - Update: 組織情報編集（組織管理者）

### オブジェクト関係図

```plantuml
@startuml
title オブジェクト関係図 - Job Board Application

object User {
  id: UUID
  email: String
  role: UserRole
  profile: UserProfile
}

object Organization {
  id: UUID
  name: String
  info: OrganizationInfo
}

object Job {
  id: UUID
  position: String
  status: JobStatus
  info: JobInfo
}

object Application {
  id: UUID
  status: ApplicationStatus
  data: ApplicationData
}

User ||--o{ Application : "応募"
Job ||--o{ Application : "受付"
Organization ||--o{ Job : "掲載"
Organization ||--o{ User : "所属"

note top of User
  ロール:
  - 求職者 (JOB_SEEKER)
  - 採用担当者 (RECRUITER)
  - 組織管理者 (ADMIN)
end note

note right of Job
  ステータス:
  - 下書き (DRAFT)
  - 公開中 (PUBLISHED)
  - 一時停止 (PAUSED)
  - 終了 (CLOSED)
end note

note bottom of Application
  選考状況:
  - 新規応募 (SUBMITTED)
  - 書類選考中 (SCREENING)
  - 面接調整中 (INTERVIEW_SCHEDULED)
  - 面接実施中 (INTERVIEW_COMPLETED)
  - 最終選考中 (FINAL_REVIEW)
  - 内定 (ACCEPTED)
  - 不採用 (REJECTED)
  - 辞退 (WITHDRAWN)
end note

@enduml
```

## 画面構成設計

### 画面遷移図

```plantuml
@startuml
title 画面遷移図

[*] --> ホーム画面
ホーム画面 --> 求人検索画面
ホーム画面 --> ログイン画面
ホーム画面 --> ユーザー登録画面

ログイン画面 --> ダッシュボード画面 : ログイン成功
ユーザー登録画面 --> ダッシュボード画面 : 登録成功

求人検索画面 --> 求人詳細画面 : 求人選択
求人詳細画面 --> 応募画面 : 応募ボタン
応募画面 --> 応募完了画面 : 応募送信

ダッシュボード画面 --> 求職者ダッシュボード : role=JOB_SEEKER
ダッシュボード画面 --> 採用担当者ダッシュボード : role=RECRUITER
ダッシュボード画面 --> 管理者ダッシュボード : role=ADMIN

採用担当者ダッシュボード --> 求人管理画面
求人管理画面 --> 求人作成画面 : 新規作成
求人管理画面 --> 求人編集画面 : 編集
求人管理画面 --> 応募者管理画面 : 応募者確認

応募者管理画面 --> 応募者詳細画面 : 応募者選択

管理者ダッシュボード --> 組織管理画面
組織管理画面 --> ユーザー管理画面

state ログイン画面 {
  [*] --> メールアドレス入力
  メールアドレス入力 --> パスワード入力
  パスワード入力 --> 認証処理
  認証処理 --> [*] : 成功
  認証処理 --> メールアドレス入力 : 失敗
}

state 求人詳細画面 {
  [*] --> 基本情報表示
  基本情報表示 --> 詳細情報表示
  詳細情報表示 --> 企業情報表示
}

state 応募画面 {
  [*] --> 基本情報入力
  基本情報入力 --> 履歴書アップロード
  履歴書アップロード --> 志望動機入力
  志望動機入力 --> 確認画面
  確認画面 --> 送信処理
  送信処理 --> [*] : 成功
}

@enduml
```

### レイアウトパターン

#### 1. 左サイドナビゲーション（ダッシュボード系画面）

```plantuml
@startuml
salt
{
  {T
    + システムロゴ                            | ユーザー情報 ▼
  }
  {+
    {S
      求人管理
      --
      応募者管理
      --
      組織管理
      --
      プロフィール
      --
      ログアウト
    } | {^
      メインコンテンツエリア
      .
      .
      .
      .
    }
  }
}
@enduml
```

#### 2. トップナビゲーション（公開画面）

```plantuml
@startuml
salt
{
  {T
    ロゴ | 求人検索 | 企業情報 | ログイン | 新規登録
  }
  {^
    メインコンテンツエリア
    .
    .
    .
    .
  }
}
@enduml
```

## 主要画面設計

### 1. 求人検索画面（コレクションビュー）

```plantuml
@startuml
salt
{
  {T
    求人検索 | フィルター条件をクリア
  }
  --
  {
    職種: "Software Engineer"
    勤務地: [東京都 ▼]
    雇用形態: [ ] 正社員 [ ] 契約社員 [ ] 派遣 
    給与: [300万] 〜 [800万] 円
    [検索実行]
  }
  --
  {T
    "325件の求人が見つかりました" | ソート: [更新日順 ▼]
  }
  --
  {^
    {
      **Software Engineer - Frontend** | 東京都 | 正社員
      ABC株式会社 | 更新日: 2025-09-10
      React, TypeScript, Next.js での開発経験者募集
      年収: 500-800万円
      [詳細] [応募]
    }
    --
    {
      **UX/UI Designer** | 大阪府 | 正社員  
      XYZ株式会社 | 更新日: 2025-09-09
      ユーザー体験設計のスペシャリスト募集
      年収: 450-650万円
      [詳細] [応募]
    }
    --
    {
      **Product Manager** | リモート | 正社員
      DEF株式会社 | 更新日: 2025-09-08  
      プロダクト戦略立案から実行まで
      年収: 600-1000万円
      [詳細] [応募]
    }
  }
  --
  < 前へ | 1 | 2 | 3 | ... | 33 | 次へ >
}
@enduml
```

### 2. 求人詳細画面（シングルビュー）

```plantuml
@startuml
salt
{
  {T
    "Software Engineer - Frontend" | [応募する] [お気に入り追加]
  }
  --
  {
    ABC株式会社 | 東京都渋谷区 | 正社員 | 更新日: 2025-09-10
  }
  --
  {S
    **職務内容**
    React、TypeScript、Next.jsを用いたWebアプリケーション開発
    UI/UXデザイナーとの協働によるユーザー体験の向上
    
    **必要スキル**
    • React、TypeScript 実務経験 2年以上
    • モダンフロントエンド開発経験
    • Git を用いたチーム開発経験
    
    **歓迎スキル** 
    • Next.js、Vercel での開発経験
    • デザインシステム構築経験
    • アジャイル開発経験
    
    **待遇・福利厚生**
    • 年収: 500-800万円（経験・スキルによる）
    • 完全週休二日制（土日祝休み）
    • フレックスタイム制
    • リモートワーク可（週2日出社）
    • 各種社会保険完備
    
    **選考プロセス**
    書類選考 → 1次面接 → 技術面接 → 最終面接
    
    **企業情報**
    設立: 2015年 | 従業員数: 150名 | 業界: IT・インターネット
    事業内容: SaaS プラットフォームの開発・運営
  }
  --
  {T
    [← 検索結果に戻る] | [応募する] [お気に入り追加]
  }
}
@enduml
```

### 3. 応募フォーム画面

```plantuml
@startuml
salt
{
  {T
    求人応募 - Software Engineer Frontend
  }
  --
  {
    **基本情報**
    氏名: "田中 太郎"
    フリガナ: "タナカ タロウ"
    メールアドレス: "tanaka@example.com"
    電話番号: "090-1234-5678"
    
    **履歴書**
    ファイル選択: [ファイルを選択] "resume.pdf"
    ※ PDF, Word形式、5MB以下
    
    **志望動機**
    {S
      React と TypeScript を用いた開発経験を活かし、
      ユーザー体験の向上に貢献したいと考えています。
      特に Next.js を用いたパフォーマンス最適化に
      興味があり、...
    }
    
    **応募確認**
    [ ] 個人情報の取り扱いに同意する
    [ ] 企業からの連絡を受け取ることに同意する
  }
  --
  {T
    [キャンセル] | [入力内容を確認] [応募する]
  }
}
@enduml
```

### 4. 採用担当者ダッシュボード

```plantuml
@startuml
salt
{
  {S
    **求人管理**
    --
    **応募者管理** 
    --
    **組織管理**
    --
    **レポート**
    --
    **設定**
  } | {^
    {T
      ダッシュボード | [新規求人作成]
    }
    --
    **統計情報**
    {
      公開中求人: 8件 | 応募総数: 45件 | 今月の応募: 12件
    }
    --
    **最近の求人**
    {
      Software Engineer | 公開中 | 応募数: 15件 | [管理]
      UX Designer | 一時停止 | 応募数: 8件 | [管理]  
      Product Manager | 下書き | 応募数: 0件 | [編集]
    }
    --
    **最近の応募**
    {
      田中太郎 | Software Engineer | 2025-09-10 | [確認]
      佐藤花子 | UX Designer | 2025-09-09 | [確認]
      山田次郎 | Software Engineer | 2025-09-09 | [確認]
    }
    --
    **アクション が必要な項目**
    {
      • 書類選考待ち: 5件
      • 面接調整が必要: 3件  
      • 最終選考中: 2件
    }
  }
}
@enduml
```

### 5. 応募者管理画面

```plantuml
@startuml
salt
{
  {S
    求人管理
    --
    **応募者管理**
    --
    組織管理
  } | {^
    {T
      応募者管理 - Software Engineer | フィルター: [全て ▼] | ソート: [応募日順 ▼]
    }
    --
    {
      **田中太郎** | 応募日: 2025-09-10 | 状況: 新規応募
      tanaka@example.com | React 3年経験
      [詳細確認] [書類選考通過] [不採用]
    }
    --
    {
      **佐藤花子** | 応募日: 2025-09-09 | 状況: 書類選考中  
      sato@example.com | TypeScript 2年経験
      [詳細確認] [面接設定] [不採用]
    }
    --
    {
      **山田次郎** | 応募日: 2025-09-09 | 状況: 面接調整中
      yamada@example.com | Next.js 1年経験  
      [詳細確認] [面接実施] [不採用]
    }
    --
    {
      **高橋美香** | 応募日: 2025-09-08 | 状況: 面接実施中
      takahashi@example.com | React 4年経験
      [詳細確認] [最終選考へ] [不採用] 
    }
  }
}
@enduml
```

### 6. 求人作成フォーム

```plantuml
@startuml
salt
{
  {S
    求人管理
    --
    応募者管理
  } | {^
    {T
      新規求人作成 | [プレビュー] [下書き保存] [公開]
    }
    --
    {
      **基本情報**
      職種名: "Software Engineer"
      部門: [エンジニアリング ▼]  
      勤務地: [東京都 ▼] [渋谷区 ▼]
      雇用形態: ( ) 正社員 ( ) 契約社員 ( ) 派遣 ( ) インターン
      
      **給与情報**
      年収範囲: [500] 〜 [800] 万円
      ( ) 年収例を表示する
      
      **職務内容**
      {S
        React、TypeScript、Next.jsを用いた
        Webアプリケーション開発をお任せします。
        
        【具体的な業務内容】
        • フロントエンド設計・実装
        • UI/UXデザイナーとの協働
        • コードレビュー・メンタリング
      }
      
      **必要スキル**
      {S
        • React、TypeScript 実務経験 2年以上
        • モダンフロントエンド開発経験  
        • Git を用いたチーム開発経験
      }
      
      **歓迎スキル**
      {S
        • Next.js、Vercel での開発経験
        • デザインシステム構築経験
        • アジャイル開発経験
      }
      
      **福利厚生・待遇**
      {S
        • 完全週休二日制（土日祝休み）
        • フレックスタイム制
        • リモートワーク可（週2日出社）
        • 各種社会保険完備
      }
      
      **応募設定**
      応募締切: [2025-10-31]
      ( ) 締切を設定しない
      
      **選考プロセス**
      書類選考 → 1次面接 → 技術面接 → 最終面接
    }
  }
}
@enduml
```

## UIコンポーネント設計

### 基本UIコンポーネント

#### 1. Button コンポーネント

```typescript
interface ButtonProps {
  variant: 'primary' | 'secondary' | 'danger' | 'ghost';
  size: 'sm' | 'md' | 'lg';
  fullWidth?: boolean;
  loading?: boolean;
  disabled?: boolean;
  children: React.ReactNode;
  onClick?: () => void;
}
```

#### 2. Card コンポーネント

```typescript
interface CardProps {
  title?: string;
  subtitle?: string;
  actions?: React.ReactNode;
  children: React.ReactNode;
  hoverable?: boolean;
  selected?: boolean;
}
```

#### 3. SearchForm コンポーネント

```typescript
interface SearchFormProps {
  filters: JobSearchFilters;
  onFiltersChange: (filters: Partial<JobSearchFilters>) => void;
  onClear: () => void;
  loading?: boolean;
}
```

### フィーチャーコンポーネント

#### 1. JobCard コンポーネント

```typescript
interface JobCardProps {
  job: Job;
  variant?: 'list' | 'grid';
  actions?: {
    onView?: (job: Job) => void;
    onApply?: (job: Job) => void;
    onEdit?: (job: Job) => void;
    onDelete?: (job: Job) => void;
  };
}
```

#### 2. ApplicationCard コンポーネント

```typescript
interface ApplicationCardProps {
  application: Application;
  job?: Job;
  actions?: {
    onView?: (application: Application) => void;
    onUpdateStatus?: (id: string, status: ApplicationStatus) => void;
    onAddNote?: (id: string, note: string) => void;
  };
  showJobInfo?: boolean;
}
```

#### 3. UserProfile コンポーネント

```typescript
interface UserProfileProps {
  user: User;
  editable?: boolean;
  onEdit?: (user: User) => void;
  onSave?: (userData: Partial<User>) => void;
}
```

## レスポンシブデザイン

### ブレークポイント設計

```css
/* モバイル（スマートフォン） */
@media (max-width: 767px) {
  .container {
    padding: 1rem;
  }
  
  .sidebar {
    display: none; /* ハンバーガーメニューに変更 */
  }
  
  .job-card {
    display: block; /* 縦積みレイアウト */
  }
}

/* タブレット */
@media (min-width: 768px) and (max-width: 1023px) {
  .container {
    padding: 1.5rem;
  }
  
  .job-grid {
    grid-template-columns: repeat(2, 1fr);
  }
}

/* デスクトップ */
@media (min-width: 1024px) {
  .container {
    padding: 2rem;
  }
  
  .job-grid {
    grid-template-columns: repeat(3, 1fr);
  }
  
  .sidebar {
    width: 250px;
  }
}
```

### モバイル対応

#### モバイル用求人検索画面

```plantuml
@startuml
salt
{
  {T
    ☰ | 求人検索 | 🔍
  }
  --
  {
    職種: "Engineer"
    [詳細検索 ▼]
  }
  --
  {^
    {
      **Software Engineer**
      ABC株式会社 | 東京都
      500-800万円
      [詳細] [応募]
    }
    --
    {
      **UX Designer** 
      XYZ株式会社 | 大阪府
      450-650万円
      [詳細] [応募]
    }
  }
}
@enduml
```

## アクセシビリティ設計

### WAI-ARIA 対応

```typescript
// 求人カードのアクセシビリティ
<article 
  role="article"
  aria-labelledby={`job-title-${job.id}`}
  aria-describedby={`job-description-${job.id}`}
>
  <h3 id={`job-title-${job.id}`}>{job.position}</h3>
  <p id={`job-description-${job.id}`}>{job.description}</p>
  <button 
    aria-label={`${job.position}の詳細を確認`}
    onClick={() => onView(job)}
  >
    詳細
  </button>
</article>

// フォームのアクセシビリティ
<form role="form" aria-labelledby="application-form-title">
  <h2 id="application-form-title">求人応募フォーム</h2>
  <fieldset>
    <legend>基本情報</legend>
    <label htmlFor="name">氏名 *</label>
    <input 
      id="name"
      type="text"
      required
      aria-required="true"
      aria-describedby="name-error"
    />
    <div id="name-error" role="alert" aria-live="polite">
      {errors.name}
    </div>
  </fieldset>
</form>
```

### キーボードナビゲーション

```typescript
// キーボードショートカット対応
const handleKeyDown = (event: KeyboardEvent) => {
  switch (event.key) {
    case 'Enter':
    case ' ':
      selectJob();
      break;
    case 'Escape':
      closeModal();
      break;
    case 'ArrowDown':
      moveToNextJob();
      break;
    case 'ArrowUp':
      moveToPreviousJob();
      break;
  }
};

// フォーカス管理
const useJobListNavigation = (jobs: Job[]) => {
  const [focusedIndex, setFocusedIndex] = useState(0);
  
  const handleArrowKey = (direction: 'up' | 'down') => {
    setFocusedIndex(prev => {
      if (direction === 'down') {
        return Math.min(prev + 1, jobs.length - 1);
      } else {
        return Math.max(prev - 1, 0);
      }
    });
  };
  
  return { focusedIndex, handleArrowKey };
};
```

## パフォーマンス最適化

### 仮想化

```typescript
// 大量求人リストの仮想化
import { FixedSizeList as List } from 'react-window';

const VirtualizedJobList: React.FC<{jobs: Job[]}> = ({ jobs }) => (
  <List
    height={600}
    itemCount={jobs.length}
    itemSize={120}
    overscanCount={5}
  >
    {({ index, style }) => (
      <div style={style}>
        <JobCard job={jobs[index]} />
      </div>
    )}
  </List>
);
```

### 画像最適化

```typescript
// 次世代画像フォーマット対応
const OptimizedImage: React.FC<{
  src: string;
  alt: string;
  width: number;
  height: number;
}> = ({ src, alt, width, height }) => (
  <picture>
    <source srcSet={`${src}.webp`} type="image/webp" />
    <source srcSet={`${src}.avif`} type="image/avif" />
    <img 
      src={src} 
      alt={alt} 
      width={width} 
      height={height}
      loading="lazy"
    />
  </picture>
);
```

## エラーハンドリング・フィードバック

### エラー状態の表示

```plantuml
@startuml
!theme blueprint
salt
{
  {T
    求人検索 | ⚠️ 検索でエラーが発生しました
  }
  --
  {
    ⚠️ ネットワークエラー
    インターネット接続を確認して、再度お試しください。
    
    [再試行] [サポートに連絡]
  }
  --
  {
    💡 検索のヒント
    • 職種名を変更してみてください
    • フィルター条件を緩和してみてください  
    • 別の地域で検索してみてください
  }
}
@enduml
```

### ローディング状態

```typescript
// スケルトンローディング
const JobCardSkeleton: React.FC = () => (
  <div className="job-card-skeleton">
    <div className="skeleton-line skeleton-title" />
    <div className="skeleton-line skeleton-company" />
    <div className="skeleton-line skeleton-location" />
    <div className="skeleton-line skeleton-salary" />
  </div>
);

// プログレス表示
const FileUploadProgress: React.FC<{progress: number}> = ({ progress }) => (
  <div className="upload-progress">
    <div className="progress-bar">
      <div 
        className="progress-fill" 
        style={{ width: `${progress}%` }}
      />
    </div>
    <span>{progress}% アップロード中...</span>
  </div>
);
```

## 国際化（i18n）対応

### 多言語対応設計

```typescript
// 言語設定
const i18nConfig = {
  defaultLocale: 'ja',
  locales: ['ja', 'en'],
  namespaces: ['common', 'job', 'application', 'error']
};

// 使用例
const JobCard: React.FC<{job: Job}> = ({ job }) => {
  const { t } = useTranslation('job');
  
  return (
    <div className="job-card">
      <h3>{job.position}</h3>
      <p>{job.company}</p>
      <button>{t('apply_button')}</button>
      <span>{t('salary_range', { min: job.salaryMin, max: job.salaryMax })}</span>
    </div>
  );
};
```

### RTL対応

```css
/* 右から左に読む言語（アラビア語等）への対応 */
[dir="rtl"] .job-card {
  text-align: right;
}

[dir="rtl"] .button-group {
  flex-direction: row-reverse;
}

[dir="rtl"] .sidebar {
  right: 0;
  left: auto;
}
```

## デザインシステム

### カラーパレット

```css
:root {
  /* Primary Colors */
  --color-primary-50: #f0f4ff;
  --color-primary-100: #e0e7ff;
  --color-primary-500: #667eea;
  --color-primary-600: #5a67d8;
  --color-primary-700: #4c51bf;
  
  /* Semantic Colors */
  --color-success: #48bb78;
  --color-warning: #ed8936;
  --color-error: #f56565;
  --color-info: #4299e1;
  
  /* Neutral Colors */
  --color-gray-50: #f7fafc;
  --color-gray-100: #edf2f7;
  --color-gray-500: #a0aec0;
  --color-gray-700: #4a5568;
  --color-gray-900: #1a202c;
}
```

### タイポグラフィ

```css
/* フォントシステム */
:root {
  --font-family-base: "Noto Sans JP", "Hiragino Kaku Gothic ProN", sans-serif;
  --font-family-mono: "Consolas", "Monaco", monospace;
  
  /* フォントサイズ */
  --font-size-xs: 0.75rem;   /* 12px */
  --font-size-sm: 0.875rem;  /* 14px */
  --font-size-base: 1rem;    /* 16px */
  --font-size-lg: 1.125rem;  /* 18px */
  --font-size-xl: 1.25rem;   /* 20px */
  --font-size-2xl: 1.5rem;   /* 24px */
  --font-size-3xl: 1.875rem; /* 30px */
  
  /* 行間 */
  --line-height-tight: 1.25;
  --line-height-normal: 1.5;
  --line-height-relaxed: 1.75;
}
```

### スペーシングシステム

```css
/* スペーシング */
:root {
  --spacing-1: 0.25rem;  /* 4px */
  --spacing-2: 0.5rem;   /* 8px */
  --spacing-3: 0.75rem;  /* 12px */
  --spacing-4: 1rem;     /* 16px */
  --spacing-5: 1.25rem;  /* 20px */
  --spacing-6: 1.5rem;   /* 24px */
  --spacing-8: 2rem;     /* 32px */
  --spacing-10: 2.5rem;  /* 40px */
  --spacing-12: 3rem;    /* 48px */
  --spacing-16: 4rem;    /* 64px */
}
```

## まとめ

React Job Board Application のUI設計は、オブジェクト指向UI設計（OOUX）の原則に基づいて以下の特徴を持つ設計としました：

### 主要設計判断

1. **オブジェクト中心設計**: 求人・応募・ユーザー・組織をコアオブジェクトとしたUI構造
2. **ロールベース UI**: 求職者・採用担当者・組織管理者それぞれに最適化されたインターフェース
3. **レスポンシブファースト**: モバイル・タブレット・デスクトップ全対応
4. **アクセシビリティ重視**: WCAG 2.1 AA準拠のユニバーサルデザイン
5. **パフォーマンス最適化**: 仮想化・遅延読み込み・画像最適化による高速表示

### UI/UX の特徴

- **直感的なナビゲーション**: オブジェクト関係に基づく自然な画面遷移
- **一貫したデザインシステム**: カラー・タイポグラフィ・スペーシングの体系化
- **効率的なワークフロー**: 求人検索から応募まで最小ステップでの完了
- **リアルタイムフィードバック**: ローディング・エラー・成功状態の明確な表示

この設計により、全てのユーザーが効率的で快適な求人活動を行える高品質なUI/UXが実現されます。

<function_calls>
<invoke name="mcp__byterover-mcp__byterover-store-knowledge">
<parameter name="messages">React Job Board Application UI設計完了

## 作成成果物
- **ui_design.md** - オブジェクト指向UI設計（OOUX）に基づくUI設計書

## UI設計の特徴
- **主要オブジェクト**: 求人（Job）、応募（Application）、ユーザー（User）、組織（Organization）
- **レイアウトパターン**: 左サイドナビ（ダッシュボード）、トップナビ（公開画面）
- **画面構成**: コレクションビュー（一覧）とシングルビュー（詳細）の組み合わせ
- **レスポンシブ対応**: モバイルファースト、3ブレークポイント対応

## 主要画面設計
1. 求人検索画面（検索フォーム + 結果一覧）
2. 求人詳細画面（職務内容・企業情報表示）
3. 応募フォーム（基本情報・履歴書・志望動機）
4. 採用担当者ダッシュボード（統計・最近の活動）
5. 応募者管理画面（選考状況管理）
6. 求人作成フォーム（詳細入力・プレビュー）

## アクセシビリティ・パフォーマンス
- WCAG 2.1 AA準拠
- キーボードナビゲーション対応
- 仮想化による大量データ表示最適化
- 画像遅延読み込み・次世代フォーマット対応

## デザインシステム
- カラーパレット（Primary・Semantic・Neutral）
- タイポグラフィシステム
- スペーシングシステム
- UIコンポーネント設計（Button・Card・SearchForm等）