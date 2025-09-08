---
title: 第5章: Feature-based Architecture
description: 
published: true
date: 2025-09-08T02:03:16.576Z
tags: 
editor: markdown
dateCreated: 2025-09-08T02:03:16.576Z
---

# 第5章: 機能モジュール設計とFeaturesアーキテクチャ

## 概要

第5章では、機能ベースのモジュール設計（Features Architecture）の実装について解説します。この章では、ビジネスロジックを機能単位で分離し、スケーラブルで保守性の高いアプリケーション構造を構築する方法を学びます。Jobs Appの求人機能を例に、型定義からコンポーネントまでの一貫した実装アプローチを実装します。

## アーキテクチャ図

```plantuml
@startuml
!theme plain

package "Chapter 5 - Features Architecture" {
  package "Jobs Feature" {
    [Types] as jobtypes
    [API Layer] as jobapi
    [Components] as jobcomponents
    [Index] as jobindex
  }
  
  package "Organizations Feature" {
    [Types] as orgtypes
    [API Layer] as orgapi
    [Components] as orgcomponents
    [Index] as orgindex
  }
  
  package "Auth Feature" {
    [Types] as authtypes
    [API Layer] as authapi
    [Components] as authcomponents
    [Index] as authindex
  }
}

package "Shared Infrastructure" {
  [Common Types] as commontypes
  [Base Components] as basecomponents
  [API Client] as apiclient
}

jobtypes --> commontypes : extends
orgtypes --> commontypes : extends
authtypes --> commontypes : extends

jobapi --> apiclient : uses
orgapi --> apiclient : uses
authapi --> apiclient : uses

jobcomponents --> basecomponents : uses
orgcomponents --> basecomponents : uses
authcomponents --> basecomponents : uses

@enduml
```

## 実装詳細

### 1. Features アーキテクチャ設計原則

#### 1.1 機能ベース分離

```plantuml
@startuml
!theme plain

package "Feature-Based Architecture" {
  [Business Domain] as domain
  [Technical Layers] as layers
  [Shared Resources] as shared
  [Integration Points] as integration
}

domain --> layers : organizes by
layers --> shared : utilizes
layers --> integration : defines
integration --> shared : coordinates

note right of domain : Jobs, Organizations, Auth\nビジネス境界での分離\nドメイン駆動設計

note right of layers : types, api, components\n技術的な責任分離\n一貫した構造

@enduml
```

#### 1.2 Feature構造設計

```
features/
├── jobs/
│   ├── api/           # データアクセス層
│   ├── components/    # UI コンポーネント
│   ├── types/         # 型定義
│   └── index.ts       # パブリックAPI
├── organizations/
│   ├── api/
│   ├── components/
│   ├── types/
│   └── index.ts
└── auth/
    ├── api/
    ├── components/
    ├── types/
    └── index.ts
```

### 2. Jobs Feature実装

#### 2.1 型定義システム

```typescript
import { Entity } from '@/types';

export type Job = Entity & {
  organizationId: string;
  position: string;
  info: string;
  location: string;
  department: string;
};

export type CreateJobData = Pick<
  Job,
  'position' | 'department' | 'location' | 'info'
>;
```

**型設計の特徴**:

```plantuml
@startuml
!theme plain

class Entity {
  +id: string
  +createdAt: Date
  +updatedAt: Date
}

class Job {
  +organizationId: string
  +position: string
  +info: string
  +location: string
  +department: string
}

class CreateJobData {
  +position: string
  +department: string
  +location: string
  +info: string
}

Entity <|-- Job : extends
Job --> CreateJobData : Pick<Job, keys>

note right of Entity : 共通エンティティ基底型\nID、タイムスタンプ\n一貫したデータ構造

note right of CreateJobData : 作成時のデータ型\nPick utility type活用\n型安全な操作

@enduml
```

#### 2.2 データモデル設計

**ドメインモデル**:
- **Job Entity**: 求人情報の完全な表現
- **CreateJobData**: 新規求人作成時の入力データ
- **組織関連**: organizationId による関連性
- **必須フィールド**: position, department, location, info

#### 2.3 型階層とユーティリティ型

```typescript
// 基底型の拡張パターン
export type UpdateJobData = Partial<CreateJobData>;

export type JobFilters = {
  department?: string;
  location?: string;
  organizationId: string;
};

export type JobSortOptions = {
  field: keyof Job;
  direction: 'asc' | 'desc';
};
```

**型の関係性**:

```plantuml
@startuml
!theme plain

package "Type Relationships" {
  [Job] as job
  [CreateJobData] as create
  [UpdateJobData] as update
  [JobFilters] as filters
  [JobSortOptions] as sort
}

job --> create : Pick<Job, keys>
create --> update : Partial<CreateJobData>
job --> filters : filter by fields
job --> sort : sort by fields

note right of create : Pick utility type\n必要フィールドのみ抽出

note right of update : Partial utility type\nオプショナルな更新

@enduml
```

### 3. Component Architecture

#### 3.1 Jobs List Component

```typescript
import { Box } from '@chakra-ui/react';

import {
  DataTable,
  DataTableProps,
} from '@/components/data-table';
import { Link } from '@/components/link';

import { Job } from '../../types';

type JobListType = 'dashboard' | 'public';

export type JobsListProps = {
  type: JobListType;
  jobs: Job[];
  isLoading?: boolean;
  organizationId: string;
};

const getTableColumns = (
  organizationId: string,
  type: JobListType
) => {
  const tableColumns: DataTableProps<Job>['columns'] = [
    {
      title: 'Position',
      field: 'position',
    },
    {
      title: 'Department',
      field: 'department',
    },
    {
      title: 'Location',
      field: 'location',
    },
    {
      title: '',
      field: 'id',
      render: ({ entry: { id } }) => {
        return (
          <Link
            href={
              type === 'public'
                ? `/organizations/${organizationId}/jobs/${id}`
                : `/dashboard/jobs/${id}`
            }
          >
            View
          </Link>
        );
      },
    },
  ];

  return tableColumns;
};

export const JobsList = ({
  jobs,
  isLoading,
  organizationId,
  type,
}: JobsListProps) => {
  const tableColumns = getTableColumns(
    organizationId,
    type
  );

  return (
    <Box data-testid="jobs-list">
      <DataTable
        isLoading={isLoading || false}
        data={jobs}
        columns={tableColumns}
      />
    </Box>
  );
};
```

**コンポーネント設計パターン**:

```plantuml
@startuml
!theme plain

class JobsList {
  +type: 'dashboard' | 'public'
  +jobs: Job[]
  +isLoading: boolean
  +organizationId: string
}

class DataTable {
  +data: T[]
  +columns: Column[]
  +isLoading: boolean
}

class TableColumn {
  +title: string
  +field: keyof T
  +render?: Function
}

JobsList --> DataTable : uses
DataTable --> TableColumn : contains multiple
JobsList --> "getTableColumns" : configures

note right of JobsList : 2つの表示モード\nダッシュボード/パブリック\n動的なルーティング

@enduml
```

#### 3.2 Create Job Form Component

```typescript
// 想定されるCreate Job Form実装
import { useForm } from 'react-hook-form';
import { Box, VStack } from '@chakra-ui/react';

import { Button } from '@/components/button';
import { FormField } from '@/components/form';

import { CreateJobData } from '../../types';

export type CreateJobFormProps = {
  onSubmit: (data: CreateJobData) => void;
  isLoading?: boolean;
};

export const CreateJobForm = ({
  onSubmit,
  isLoading,
}: CreateJobFormProps) => {
  const {
    register,
    handleSubmit,
    formState: { errors },
  } = useForm<CreateJobData>();

  return (
    <Box>
      <form onSubmit={handleSubmit(onSubmit)}>
        <VStack spacing={4}>
          <FormField
            label="Position"
            error={errors.position}
            {...register('position', { required: 'Position is required' })}
          />
          <FormField
            label="Department"
            error={errors.department}
            {...register('department', { required: 'Department is required' })}
          />
          <FormField
            label="Location"
            error={errors.location}
            {...register('location', { required: 'Location is required' })}
          />
          <FormField
            as="textarea"
            label="Job Information"
            error={errors.info}
            {...register('info', { required: 'Job info is required' })}
          />
          <Button
            type="submit"
            isLoading={isLoading}
            w="full"
          >
            Create Job
          </Button>
        </VStack>
      </form>
    </Box>
  );
};
```

#### 3.3 Job Info Display Components

```plantuml
@startuml
!theme plain

package "Job Display Components" {
  [PublicJobInfo] as publicinfo
  [DashboardJobInfo] as dashboardinfo
  [JobCard] as jobcard
  [JobDetails] as details
}

package "Display Modes" {
  [Read-only View] as readonly
  [Editable View] as editable
  [Compact View] as compact
  [Detailed View] as detailed
}

publicinfo --> readonly : implements
publicinfo --> detailed : shows
dashboardinfo --> editable : supports
dashboardinfo --> detailed : shows
jobcard --> compact : implements
details --> detailed : implements

@enduml
```

### 4. Feature API Layer（Chapter-06で実装）

#### 4.1 API設計パターン

```plantuml
@startuml
!theme plain

package "API Layer Architecture" {
  [API Functions] as apifuncs
  [React Query Hooks] as hooks
  [Error Handling] as errors
  [Data Transformation] as transform
}

package "API Client Integration" {
  [HTTP Client] as http
  [Request Configuration] as config
  [Response Processing] as response
  [Cache Management] as cache
}

apifuncs --> http : uses
hooks --> apifuncs : wraps
hooks --> cache : manages
errors --> response : handles
transform --> response : processes

@enduml
```

#### 4.2 想定されるAPI実装

```typescript
// Chapter-06で実装される予定のAPI層
import { useQuery, useMutation } from '@tanstack/react-query';
import { apiClient } from '@/lib/api-client';

// GET Jobs
export const getJobs = (organizationId: string): Promise<Job[]> => {
  return apiClient.get('/jobs', {
    params: { organizationId }
  });
};

export const useJobs = (organizationId: string) => {
  return useQuery({
    queryKey: ['jobs', organizationId],
    queryFn: () => getJobs(organizationId),
    enabled: !!organizationId,
  });
};

// CREATE Job
export const createJob = (data: CreateJobData): Promise<Job> => {
  return apiClient.post('/jobs', data);
};

export const useCreateJob = () => {
  return useMutation({
    mutationFn: createJob,
    onSuccess: () => {
      // Cache invalidation logic
    },
  });
};
```

### 5. Feature Index (Public API)

#### 5.1 Feature Export Strategy

```typescript
// features/jobs/index.ts - Public API
// API Layer
export * from './api/get-job';
export * from './api/get-jobs';
export * from './api/create-job';

// Components
export * from './components/jobs-list';
export * from './components/create-job-form';
export * from './components/dashboard-job-info';
export * from './components/public-job-info';

// Types
export * from './types';
```

**Export設計原則**:

```plantuml
@startuml
!theme plain

package "Export Strategy" {
  [Named Exports] as named
  [Barrel Exports] as barrel
  [Type Exports] as types
  [Component Exports] as components
}

package "Import Patterns" {
  [Feature Imports] as featureimports
  [Selective Imports] as selective
  [Type-only Imports] as typeonly
}

named --> featureimports : enables
barrel --> featureimports : simplifies
types --> typeonly : supports
components --> selective : allows

note right of barrel : index.tsによる\n統一されたエクスポート\n簡潔なimport文

@enduml
```

#### 5.2 Feature間の依存関係

```plantuml
@startuml
!theme plain

package "Feature Dependencies" {
  [Jobs Feature] as jobs
  [Organizations Feature] as orgs
  [Auth Feature] as auth
  [Shared Types] as shared
}

jobs --> orgs : organizationId reference
jobs --> auth : user context
auth --> orgs : user organization
jobs --> shared : extends Entity
orgs --> shared : extends Entity
auth --> shared : extends Entity

note right of shared : 共通型定義\nEntity, ID types\nAPI response types

@enduml
```

### 6. 状態管理パターン（Chapter-06で拡張）

#### 6.1 Local State vs Global State

```plantuml
@startuml
!theme plain

package "State Management Strategy" {
  [Component State] as local
  [Feature State] as feature
  [Global State] as global
  [Server State] as server
}

package "State Scopes" {
  [Form State] as form
  [UI State] as ui
  [Business State] as business
  [Cache State] as cache
}

local --> form : manages
local --> ui : manages
feature --> business : manages
global --> business : shares
server --> cache : manages

@enduml
```

#### 6.2 想定される状態管理

```typescript
// Feature-level state (Chapter-06)
export const useJobsState = () => {
  const [filters, setFilters] = useState<JobFilters>({});
  const [sortOptions, setSortOptions] = useState<JobSortOptions>();
  
  return {
    filters,
    setFilters,
    sortOptions,
    setSortOptions,
  };
};

// Global state integration
export const useJobsWithGlobalState = () => {
  const { user } = useAuth();
  const localState = useJobsState();
  const jobsQuery = useJobs(user?.organizationId);
  
  return {
    ...localState,
    ...jobsQuery,
  };
};
```

### 7. Testing Strategy

#### 7.1 Feature Testing Architecture

```plantuml
@startuml
!theme plain

package "Testing Layers" {
  [Unit Tests] as unit
  [Integration Tests] as integration
  [Feature Tests] as feature
  [E2E Tests] as e2e
}

package "Test Scopes" {
  [Type Tests] as types
  [Component Tests] as components
  [API Tests] as api
  [Workflow Tests] as workflow
}

unit --> types : validates
unit --> components : tests
integration --> api : mocks
feature --> workflow : covers
e2e --> workflow : validates

@enduml
```

#### 7.2 Feature Testing Examples

```typescript
// Type testing
describe('Job Types', () => {
  it('should create valid Job type', () => {
    const job: Job = {
      id: '1',
      organizationId: 'org-1',
      position: 'Developer',
      department: 'Engineering',
      location: 'Remote',
      info: 'Job description',
      createdAt: new Date(),
      updatedAt: new Date(),
    };
    
    expect(job.position).toBe('Developer');
  });
});

// Component testing
describe('JobsList', () => {
  it('renders job list correctly', () => {
    const jobs = [mockJob1, mockJob2];
    render(
      <JobsList 
        jobs={jobs} 
        type="dashboard" 
        organizationId="org-1" 
      />
    );
    
    expect(screen.getByTestId('jobs-list')).toBeInTheDocument();
  });
});
```

### 8. Performance Considerations

#### 8.1 Feature Loading Strategy

```plantuml
@startuml
!theme plain

package "Performance Optimizations" {
  [Code Splitting] as splitting
  [Lazy Loading] as lazy
  [Selective Imports] as imports
  [Bundle Analysis] as bundle
}

package "Loading Patterns" {
  [Route-based Splitting] as route
  [Component-based Splitting] as component
  [Feature-based Splitting] as featuresplitting
}

splitting --> route : implements
splitting --> component : implements
splitting --> featuresplitting : enables
lazy --> component : defers loading
imports --> bundle : optimizes

@enduml
```

#### 8.2 Feature Optimization

```typescript
// Lazy loading features
const JobsFeature = lazy(() => import('@/features/jobs'));
const OrganizationsFeature = lazy(() => import('@/features/organizations'));

// Route-based code splitting
export const AppRoutes = () => (
  <Routes>
    <Route 
      path="/jobs/*" 
      element={
        <Suspense fallback={<Loading />}>
          <JobsFeature />
        </Suspense>
      } 
    />
  </Routes>
);
```

### 9. Documentation Strategy

#### 9.1 Feature Documentation

```plantuml
@startuml
!theme plain

package "Documentation Types" {
  [README] as readme
  [API Docs] as api
  [Component Docs] as components
  [Type Docs] as types
}

package "Documentation Tools" {
  [Storybook] as storybook
  [TypeDoc] as typedoc
  [JSDoc] as jsdoc
}

components --> storybook : showcased in
types --> typedoc : generated by
api --> jsdoc : documented with
readme --> api : references
readme --> components : describes

@enduml
```

#### 9.2 Feature README Template

```markdown
# Jobs Feature

## Overview
Handles all job-related functionality including listing, creation, and management.

## Structure
- `/api` - Data access layer
- `/components` - UI components  
- `/types` - TypeScript definitions
- `/index.ts` - Public API

## Usage
```typescript
import { JobsList, useJobs, Job } from '@/features/jobs';
```

## Components
- `JobsList` - Display jobs in table format
- `CreateJobForm` - Form for creating new jobs
- `JobInfo` - Display job details

## API
- `useJobs(organizationId)` - Fetch jobs for organization
- `useCreateJob()` - Create new job mutation
```

## 設計原則とベストプラクティス

### 1. Feature Design Principles

```plantuml
@startuml
!theme plain

package "Design Principles" {
  [Domain Separation] as domain
  [Loose Coupling] as coupling
  [High Cohesion] as cohesion
  [Clear Interfaces] as interfaces
}

domain : ビジネス境界での分離
domain : 機能の独立性
domain : ドメインエキスパートとの対話

coupling : Feature間の依存最小化
coupling : 共通インフラの活用
coupling : インターフェースベース設計

cohesion : 関連機能のグループ化
cohesion : 単一責任原則
cohesion : 内部結合度の最大化

interfaces : 明確なパブリックAPI
interfaces : 型安全な契約
interfaces : バージョニング対応

@enduml
```

### 2. Scalability Considerations

```plantuml
@startuml
!theme plain

package "Scalability Factors" {
  [Team Scaling] as team
  [Code Scaling] as code
  [Performance Scaling] as performance
  [Maintenance Scaling] as maintenance
}

team : 独立したチーム開発
team : Feature ownership
team : 並行開発

code : 新機能の追加容易性
code : 既存機能の拡張
code : コードの再利用

performance : バンドルサイズ管理
performance : レンダリング最適化
performance : メモリ使用量

maintenance : テストの維持
maintenance : リファクタリング
maintenance : 技術的負債の管理

@enduml
```

## まとめ

Chapter-05では、機能ベースのモジュール設計（Features Architecture）の基盤を構築しました：

**主要な実装成果**:
1. **Feature構造**: jobs, organizations, auth の機能分離
2. **型定義システム**: TypeScript による厳密な型管理
3. **コンポーネント設計**: 機能特化したUIコンポーネント
4. **パブリックAPI**: 明確なFeature間インターフェース
5. **スケーラビリティ**: 新機能追加の容易性

**設計の特徴**:
- **ドメイン駆動**: ビジネス機能による分離
- **一貫性**: 各Featureの統一された構造
- **独立性**: Feature間の疎結合
- **拡張性**: 新しいFeatureの追加が容易

次章では、この基盤の上にAPIレイヤーと状態管理を実装し、データフローとビジネスロジックを完成させていきます。
