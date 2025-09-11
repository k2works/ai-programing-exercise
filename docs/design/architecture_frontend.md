# フロントエンドアーキテクチャ設計書 - 会議室予約システム

## 1. アーキテクチャ決定概要

### 1.1 プロジェクト要件分析

**プロジェクト規模**: 中規模
- **判定根拠**: 
  - 主要機能数: 5-7個のエピック（予約管理、会員管理、お問い合わせ等）
  - 想定ユーザー数: 100-500名程度
  - 開発期間: 3-6ヶ月程度の継続開発

**ユーザー体験要件**:
- **SEO要件**: 低（内部ツール・会員制システム）
- **パフォーマンス要件**: 中（レスポンシブな操作性が重要）
- **インタラクティビティ**: 高（リアルタイムな予約状況、動的UI）

**更新頻度**: 中頻度
- 予約状況の動的更新
- リアルタイムな空室状況表示
- ユーザーインタラクション重視

### 1.2 選択されたアーキテクチャ

**レンダリング戦略**: SPA（Single Page Application）
- **選択理由**: 内部ツール・ダッシュボード的性質で高いインタラクティビティが重要
- **技術選択**: React 18 + TypeScript

**プロジェクト構造**: 標準11フォルダ構成
- **選択理由**: 中規模プロジェクトで将来的な拡張性を考慮

**状態管理**: Zustand + React Query
- **選択理由**: 
  - Zustand: シンプルなクライアント状態管理
  - React Query: サーバー状態とキャッシュ管理に特化

**スタイリング**: CSS Modules + Tailwind CSS
- **選択理由**: パフォーマンス重視でスタイルの再利用性と保守性を両立

## 2. プロジェクト構造設計

### 2.1 ディレクトリ構成

```
src/
├── components/              # (1) 共通UIコンポーネント
│   ├── ui/                  # 基本UIパーツ
│   │   ├── Button/
│   │   ├── Input/
│   │   ├── Modal/
│   │   ├── Card/
│   │   ├── Table/
│   │   └── DatePicker/
│   └── layout/              # レイアウトコンポーネント
│       ├── Header/
│       ├── Sidebar/
│       ├── Footer/
│       └── Navigation/
├── config/                  # (2) アプリケーション設定
│   ├── constants.ts         # 定数定義
│   ├── env.ts               # 環境変数管理
│   ├── api.ts               # API設定
│   └── routes.ts            # ルート定義
├── features/                # (3) フィーチャー単位のコンポーネント
│   ├── auth/                # 認証機能
│   │   ├── components/
│   │   ├── hooks/
│   │   ├── services/
│   │   └── types.ts
│   ├── reservation/         # 予約機能
│   │   ├── components/
│   │   │   ├── ReservationForm/
│   │   │   ├── ReservationList/
│   │   │   ├── ReservationCalendar/
│   │   │   └── ReservationDetails/
│   │   ├── hooks/
│   │   │   ├── useReservations.ts
│   │   │   ├── useReservationForm.ts
│   │   │   └── useReservationSearch.ts
│   │   ├── services/
│   │   │   ├── reservationApi.ts
│   │   │   └── reservationValidation.ts
│   │   └── types.ts
│   ├── room/                # 会議室管理機能
│   │   ├── components/
│   │   │   ├── RoomSearch/
│   │   │   ├── RoomCard/
│   │   │   └── RoomDetails/
│   │   ├── hooks/
│   │   ├── services/
│   │   └── types.ts
│   ├── member/              # 会員管理機能
│   └── inquiry/             # お問い合わせ機能
├── layouts/                 # (4) アプリケーションレイアウト
│   ├── AppLayout.tsx        # メインアプリレイアウト
│   ├── AuthLayout.tsx       # 認証画面レイアウト
│   └── DashboardLayout.tsx  # ダッシュボードレイアウト
├── lib/                     # (5) 外部ライブラリ設定・ユーティリティ
│   ├── auth.ts              # 認証ライブラリ設定
│   ├── api-client.ts        # API クライアント設定
│   ├── date.ts              # 日付処理ユーティリティ
│   └── validation.ts        # バリデーション設定
├── pages/                   # (6) ページコンポーネント
│   ├── LoginPage.tsx
│   ├── DashboardPage.tsx
│   ├── ReservationPage.tsx
│   ├── RoomManagementPage.tsx
│   └── ProfilePage.tsx
├── providers/               # (7) Context Provider
│   ├── AuthProvider.tsx     # 認証プロバイダー
│   ├── ThemeProvider.tsx    # テーマプロバイダー
│   ├── QueryProvider.tsx    # React Query プロバイダー
│   └── AppProviders.tsx     # 統合プロバイダー
├── stores/                  # (8) 状態管理
│   ├── authStore.ts         # 認証状態
│   ├── uiStore.ts           # UI状態（モーダル、通知等）
│   ├── reservationStore.ts  # 予約関連状態
│   └── index.ts             # ストア統合
├── testing/                 # (9) テスト用ユーティリティ
│   ├── setup.ts             # テスト環境設定
│   ├── mocks/               # モックデータ
│   │   ├── handlers.ts      # MSWハンドラー
│   │   └── data.ts          # テストデータ
│   └── utils.ts             # テストヘルパー
├── types/                   # (10) TypeScript型定義
│   ├── api.ts               # API型定義
│   ├── auth.ts              # 認証関連型
│   ├── reservation.ts       # 予約関連型
│   └── common.ts            # 共通型定義
└── utils/                   # (11) 汎用ユーティリティ関数
    ├── format.ts            # フォーマット関数
    ├── validation.ts        # バリデーション関数
    ├── date.ts              # 日付操作
    └── storage.ts           # ローカルストレージ管理
```

### 2.2 技術スタック詳細

#### 2.2.1 コア技術

**基盤技術**
```json
{
  "react": "^18.3.0",
  "typescript": "^5.4.0",
  "vite": "^5.2.0"
}
```

**ルーティング・状態管理**
```json
{
  "react-router-dom": "^6.23.0",
  "zustand": "^4.5.0",
  "@tanstack/react-query": "^5.28.0"
}
```

**UI・スタイリング**
```json
{
  "tailwindcss": "^3.4.0",
  "clsx": "^2.1.0",
  "lucide-react": "^0.365.0"
}
```

**フォーム・バリデーション**
```json
{
  "react-hook-form": "^7.51.0",
  "zod": "^3.22.0",
  "@hookform/resolvers": "^3.3.0"
}
```

#### 2.2.2 開発・テストツール

**開発ツール**
```json
{
  "@vitejs/plugin-react": "^4.2.0",
  "eslint": "^8.57.0",
  "prettier": "^3.2.0",
  "@typescript-eslint/eslint-plugin": "^7.6.0"
}
```

**テストツール**
```json
{
  "vitest": "^1.4.0",
  "@testing-library/react": "^14.2.0",
  "@testing-library/jest-dom": "^6.4.0",
  "msw": "^2.2.0"
}
```

## 3. コンポーネント設計パターン

### 3.1 Container/Presentational パターン

#### 3.1.1 予約機能の実装例

**Container Component（ロジック担当）**
```typescript
// features/reservation/components/ReservationListContainer.tsx
import { useReservations } from '../hooks/useReservations';
import { ReservationList } from './ReservationList';

export function ReservationListContainer() {
  const { 
    reservations, 
    loading, 
    error,
    createReservation,
    cancelReservation 
  } = useReservations();

  const handleCreate = useCallback((data: CreateReservationData) => {
    createReservation.mutate(data);
  }, [createReservation]);

  const handleCancel = useCallback((id: string) => {
    cancelReservation.mutate(id);
  }, [cancelReservation]);

  return (
    <ReservationList
      reservations={reservations}
      loading={loading}
      error={error}
      onCreate={handleCreate}
      onCancel={handleCancel}
    />
  );
}
```

**Presentational Component（UI担当）**
```typescript
// features/reservation/components/ReservationList.tsx
import { Card } from '@/components/ui/Card';
import { Button } from '@/components/ui/Button';
import { LoadingSpinner } from '@/components/ui/LoadingSpinner';
import type { Reservation, CreateReservationData } from '../types';

interface Props {
  reservations: Reservation[];
  loading: boolean;
  error: Error | null;
  onCreate: (data: CreateReservationData) => void;
  onCancel: (id: string) => void;
}

export function ReservationList({ 
  reservations, 
  loading, 
  error, 
  onCreate, 
  onCancel 
}: Props) {
  if (loading) return <LoadingSpinner />;
  if (error) return <ErrorMessage error={error} />;

  return (
    <div className="space-y-4">
      <div className="flex justify-between items-center">
        <h2 className="text-2xl font-bold">予約一覧</h2>
        <Button onClick={() => onCreate({})}>新規予約</Button>
      </div>
      
      {reservations.map(reservation => (
        <ReservationCard
          key={reservation.id}
          reservation={reservation}
          onCancel={onCancel}
        />
      ))}
    </div>
  );
}
```

### 3.2 Custom Hooks パターン

#### 3.2.1 ビジネスロジックの分離

```typescript
// features/reservation/hooks/useReservations.ts
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query';
import { reservationApi } from '../services/reservationApi';
import { useAuthStore } from '@/stores/authStore';
import { useToast } from '@/hooks/useToast';

export function useReservations() {
  const { user } = useAuthStore();
  const queryClient = useQueryClient();
  const { showToast } = useToast();

  const reservationsQuery = useQuery({
    queryKey: ['reservations', user?.id],
    queryFn: () => reservationApi.getReservations(user!.id),
    enabled: !!user,
  });

  const createReservationMutation = useMutation({
    mutationFn: reservationApi.createReservation,
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['reservations'] });
      showToast('予約が作成されました', 'success');
    },
    onError: (error) => {
      showToast('予約の作成に失敗しました', 'error');
    },
  });

  const cancelReservationMutation = useMutation({
    mutationFn: reservationApi.cancelReservation,
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['reservations'] });
      showToast('予約がキャンセルされました', 'success');
    },
    onError: (error) => {
      showToast('キャンセルに失敗しました', 'error');
    },
  });

  return {
    reservations: reservationsQuery.data ?? [],
    loading: reservationsQuery.isLoading,
    error: reservationsQuery.error,
    createReservation: createReservationMutation,
    cancelReservation: cancelReservationMutation,
  };
}
```

### 3.3 Form Management パターン

```typescript
// features/reservation/hooks/useReservationForm.ts
import { useForm } from 'react-hook-form';
import { zodResolver } from '@hookform/resolvers/zod';
import { z } from 'zod';

const reservationSchema = z.object({
  roomId: z.string().min(1, '会議室を選択してください'),
  startTime: z.date({
    required_error: '開始時刻を入力してください',
  }),
  endTime: z.date({
    required_error: '終了時刻を入力してください',
  }),
  purpose: z.string().max(500, '目的は500文字以内で入力してください'),
}).refine((data) => data.endTime > data.startTime, {
  message: '終了時刻は開始時刻より後である必要があります',
  path: ['endTime'],
});

type ReservationFormData = z.infer<typeof reservationSchema>;

export function useReservationForm() {
  const form = useForm<ReservationFormData>({
    resolver: zodResolver(reservationSchema),
    defaultValues: {
      roomId: '',
      purpose: '',
    },
  });

  const validateTimeSlot = useCallback((startTime: Date, endTime: Date) => {
    const now = new Date();
    const twoHoursLater = new Date(now.getTime() + 2 * 60 * 60 * 1000);
    
    if (startTime < twoHoursLater) {
      form.setError('startTime', {
        message: '予約は2時間以上先から可能です',
      });
      return false;
    }
    
    return true;
  }, [form]);

  return {
    ...form,
    validateTimeSlot,
  };
}
```

## 4. 状態管理設計

### 4.1 Zustand による Client State 管理

#### 4.1.1 認証状態管理

```typescript
// stores/authStore.ts
import { create } from 'zustand';
import { persist } from 'zustand/middleware';

interface User {
  id: string;
  name: string;
  email: string;
  role: 'USER' | 'STAFF' | 'ADMIN';
}

interface AuthState {
  user: User | null;
  token: string | null;
  isAuthenticated: boolean;
  login: (user: User, token: string) => void;
  logout: () => void;
  updateUser: (updates: Partial<User>) => void;
}

export const useAuthStore = create<AuthState>()(
  persist(
    (set, get) => ({
      user: null,
      token: null,
      isAuthenticated: false,

      login: (user, token) => set({
        user,
        token,
        isAuthenticated: true,
      }),

      logout: () => set({
        user: null,
        token: null,
        isAuthenticated: false,
      }),

      updateUser: (updates) => {
        const { user } = get();
        if (user) {
          set({ user: { ...user, ...updates } });
        }
      },
    }),
    {
      name: 'auth-storage',
      partialize: (state) => ({ 
        user: state.user, 
        token: state.token,
        isAuthenticated: state.isAuthenticated,
      }),
    }
  )
);
```

#### 4.1.2 UI状態管理

```typescript
// stores/uiStore.ts
import { create } from 'zustand';

interface UIState {
  theme: 'light' | 'dark';
  sidebarOpen: boolean;
  notifications: Notification[];
  modals: {
    reservationForm: boolean;
    confirmDialog: boolean;
  };
  toggleSidebar: () => void;
  setTheme: (theme: 'light' | 'dark') => void;
  addNotification: (notification: Omit<Notification, 'id'>) => void;
  removeNotification: (id: string) => void;
  openModal: (modal: keyof UIState['modals']) => void;
  closeModal: (modal: keyof UIState['modals']) => void;
}

export const useUIStore = create<UIState>((set, get) => ({
  theme: 'light',
  sidebarOpen: false,
  notifications: [],
  modals: {
    reservationForm: false,
    confirmDialog: false,
  },

  toggleSidebar: () => set((state) => ({ 
    sidebarOpen: !state.sidebarOpen 
  })),

  setTheme: (theme) => set({ theme }),

  addNotification: (notification) => set((state) => ({
    notifications: [
      ...state.notifications,
      { ...notification, id: crypto.randomUUID() }
    ]
  })),

  removeNotification: (id) => set((state) => ({
    notifications: state.notifications.filter(n => n.id !== id)
  })),

  openModal: (modal) => set((state) => ({
    modals: { ...state.modals, [modal]: true }
  })),

  closeModal: (modal) => set((state) => ({
    modals: { ...state.modals, [modal]: false }
  })),
}));
```

### 4.2 React Query による Server State 管理

#### 4.2.1 API Client 設定

```typescript
// lib/api-client.ts
import axios, { AxiosRequestConfig } from 'axios';
import { useAuthStore } from '@/stores/authStore';

const API_BASE_URL = import.meta.env.VITE_API_BASE_URL || 'http://localhost:8080/api';

export const apiClient = axios.create({
  baseURL: API_BASE_URL,
  timeout: 10000,
  headers: {
    'Content-Type': 'application/json',
  },
});

// Request interceptor for authentication
apiClient.interceptors.request.use(
  (config) => {
    const { token } = useAuthStore.getState();
    if (token) {
      config.headers.Authorization = `Bearer ${token}`;
    }
    return config;
  },
  (error) => Promise.reject(error)
);

// Response interceptor for error handling
apiClient.interceptors.response.use(
  (response) => response,
  (error) => {
    if (error.response?.status === 401) {
      useAuthStore.getState().logout();
      window.location.href = '/login';
    }
    return Promise.reject(error);
  }
);
```

#### 4.2.2 API Service Layer

```typescript
// features/reservation/services/reservationApi.ts
import { apiClient } from '@/lib/api-client';
import type { 
  Reservation, 
  CreateReservationData, 
  UpdateReservationData,
  ReservationSearchParams 
} from '../types';

export const reservationApi = {
  getReservations: async (userId: string): Promise<Reservation[]> => {
    const response = await apiClient.get(`/reservations?userId=${userId}`);
    return response.data;
  },

  getReservation: async (id: string): Promise<Reservation> => {
    const response = await apiClient.get(`/reservations/${id}`);
    return response.data;
  },

  createReservation: async (data: CreateReservationData): Promise<Reservation> => {
    const response = await apiClient.post('/reservations', data);
    return response.data;
  },

  updateReservation: async (id: string, data: UpdateReservationData): Promise<Reservation> => {
    const response = await apiClient.put(`/reservations/${id}`, data);
    return response.data;
  },

  cancelReservation: async (id: string): Promise<void> => {
    await apiClient.delete(`/reservations/${id}`);
  },

  searchReservations: async (params: ReservationSearchParams): Promise<Reservation[]> => {
    const response = await apiClient.get('/reservations/search', { params });
    return response.data;
  },
};
```

## 5. UI/UXデザインシステム

### 5.1 デザイントークン

```typescript
// config/design-tokens.ts
export const designTokens = {
  colors: {
    primary: {
      50: '#eff6ff',
      500: '#3b82f6',
      600: '#2563eb',
      700: '#1d4ed8',
      900: '#1e3a8a',
    },
    semantic: {
      success: '#10b981',
      warning: '#f59e0b',
      error: '#ef4444',
      info: '#3b82f6',
    },
  },
  spacing: {
    xs: '0.25rem',
    sm: '0.5rem',
    md: '1rem',
    lg: '1.5rem',
    xl: '2rem',
  },
  typography: {
    fontFamily: {
      sans: ['Inter', 'system-ui', 'sans-serif'],
    },
    fontSize: {
      xs: '0.75rem',
      sm: '0.875rem',
      base: '1rem',
      lg: '1.125rem',
      xl: '1.25rem',
    },
  },
} as const;
```

### 5.2 共通UIコンポーネント

#### 5.2.1 Button コンポーネント

```typescript
// components/ui/Button/Button.tsx
import { ButtonHTMLAttributes, forwardRef } from 'react';
import { clsx } from 'clsx';
import styles from './Button.module.css';

interface ButtonProps extends ButtonHTMLAttributes<HTMLButtonElement> {
  variant?: 'primary' | 'secondary' | 'danger' | 'ghost';
  size?: 'sm' | 'md' | 'lg';
  loading?: boolean;
  leftIcon?: React.ReactNode;
  rightIcon?: React.ReactNode;
}

export const Button = forwardRef<HTMLButtonElement, ButtonProps>(
  ({ 
    children, 
    variant = 'primary', 
    size = 'md', 
    loading = false,
    leftIcon,
    rightIcon,
    className,
    disabled,
    ...props 
  }, ref) => {
    return (
      <button
        ref={ref}
        className={clsx(
          styles.button,
          styles[variant],
          styles[size],
          {
            [styles.loading]: loading,
          },
          className
        )}
        disabled={disabled || loading}
        {...props}
      >
        {loading && <LoadingSpinner className={styles.spinner} />}
        {leftIcon && <span className={styles.leftIcon}>{leftIcon}</span>}
        {children}
        {rightIcon && <span className={styles.rightIcon}>{rightIcon}</span>}
      </button>
    );
  }
);
```

```css
/* components/ui/Button/Button.module.css */
.button {
  @apply inline-flex items-center justify-center rounded-md font-medium transition-colors focus:outline-none focus:ring-2 focus:ring-offset-2 disabled:opacity-50 disabled:pointer-events-none;
}

.primary {
  @apply bg-blue-600 text-white hover:bg-blue-700 focus:ring-blue-500;
}

.secondary {
  @apply bg-gray-200 text-gray-900 hover:bg-gray-300 focus:ring-gray-500;
}

.danger {
  @apply bg-red-600 text-white hover:bg-red-700 focus:ring-red-500;
}

.ghost {
  @apply text-gray-900 hover:bg-gray-100 focus:ring-gray-500;
}

.sm {
  @apply h-8 px-3 text-sm;
}

.md {
  @apply h-10 px-4 text-sm;
}

.lg {
  @apply h-12 px-6 text-base;
}

.loading {
  @apply cursor-wait;
}

.leftIcon {
  @apply mr-2 -ml-1;
}

.rightIcon {
  @apply ml-2 -mr-1;
}

.spinner {
  @apply mr-2 h-4 w-4 animate-spin;
}
```

#### 5.2.2 Form Input コンポーネント

```typescript
// components/ui/Input/Input.tsx
import { forwardRef, InputHTMLAttributes } from 'react';
import { clsx } from 'clsx';
import styles from './Input.module.css';

interface InputProps extends InputHTMLAttributes<HTMLInputElement> {
  label?: string;
  error?: string;
  helpText?: string;
  leftIcon?: React.ReactNode;
  rightIcon?: React.ReactNode;
}

export const Input = forwardRef<HTMLInputElement, InputProps>(
  ({ 
    label, 
    error, 
    helpText, 
    leftIcon, 
    rightIcon, 
    className,
    ...props 
  }, ref) => {
    return (
      <div className={styles.container}>
        {label && (
          <label htmlFor={props.id} className={styles.label}>
            {label}
          </label>
        )}
        
        <div className={styles.inputWrapper}>
          {leftIcon && (
            <div className={styles.leftIcon}>{leftIcon}</div>
          )}
          
          <input
            ref={ref}
            className={clsx(
              styles.input,
              {
                [styles.error]: error,
                [styles.withLeftIcon]: leftIcon,
                [styles.withRightIcon]: rightIcon,
              },
              className
            )}
            {...props}
          />
          
          {rightIcon && (
            <div className={styles.rightIcon}>{rightIcon}</div>
          )}
        </div>
        
        {error && <p className={styles.errorText}>{error}</p>}
        {helpText && !error && (
          <p className={styles.helpText}>{helpText}</p>
        )}
      </div>
    );
  }
);
```

## 6. パフォーマンス最適化

### 6.1 コード分割とLazy Loading

```typescript
// pages/index.tsx
import { lazy, Suspense } from 'react';
import { Routes, Route } from 'react-router-dom';
import { LoadingSpinner } from '@/components/ui/LoadingSpinner';

// ページコンポーネントの遅延読み込み
const DashboardPage = lazy(() => import('./DashboardPage'));
const ReservationPage = lazy(() => import('./ReservationPage'));
const RoomManagementPage = lazy(() => import('./RoomManagementPage'));

export function AppRouter() {
  return (
    <Suspense fallback={<LoadingSpinner />}>
      <Routes>
        <Route path="/dashboard" element={<DashboardPage />} />
        <Route path="/reservations" element={<ReservationPage />} />
        <Route path="/rooms" element={<RoomManagementPage />} />
      </Routes>
    </Suspense>
  );
}
```

### 6.2 メモ化とパフォーマンス最適化

```typescript
// features/reservation/components/ReservationCard.tsx
import { memo } from 'react';
import type { Reservation } from '../types';

interface Props {
  reservation: Reservation;
  onCancel: (id: string) => void;
}

export const ReservationCard = memo<Props>(({ reservation, onCancel }) => {
  const handleCancel = useCallback(() => {
    onCancel(reservation.id);
  }, [onCancel, reservation.id]);

  return (
    <Card className="p-4">
      <div className="flex justify-between items-start">
        <div>
          <h3 className="font-semibold">{reservation.roomName}</h3>
          <p className="text-gray-600">
            {formatDateTime(reservation.startTime)} - {formatDateTime(reservation.endTime)}
          </p>
          <p className="text-sm text-gray-500">{reservation.purpose}</p>
        </div>
        
        {reservation.status === 'CONFIRMED' && (
          <Button 
            variant="danger" 
            size="sm" 
            onClick={handleCancel}
          >
            キャンセル
          </Button>
        )}
      </div>
    </Card>
  );
}, (prevProps, nextProps) => {
  return prevProps.reservation.id === nextProps.reservation.id &&
         prevProps.reservation.status === nextProps.reservation.status;
});
```

### 6.3 Virtual Scrolling（大量データ対応）

```typescript
// components/ui/VirtualList/VirtualList.tsx
import { FixedSizeList as List, ListChildComponentProps } from 'react-window';

interface VirtualListProps<T> {
  items: T[];
  itemHeight: number;
  height: number;
  renderItem: (item: T, index: number) => React.ReactNode;
}

export function VirtualList<T>({ 
  items, 
  itemHeight, 
  height, 
  renderItem 
}: VirtualListProps<T>) {
  const Row = ({ index, style }: ListChildComponentProps) => (
    <div style={style}>
      {renderItem(items[index], index)}
    </div>
  );

  return (
    <List
      height={height}
      itemCount={items.length}
      itemSize={itemHeight}
      width="100%"
    >
      {Row}
    </List>
  );
}
```

## 7. テスト戦略

### 7.1 テスト構成

**テストピラミッド**:
- ユニットテスト（70%）: hooks、utils、components
- 統合テスト（25%）: フィーチャーレベルのテスト
- E2Eテスト（5%）: クリティカルユーザーフロー

### 7.2 単体テスト例

```typescript
// features/reservation/hooks/useReservations.test.tsx
import { renderHook, waitFor } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { useReservations } from './useReservations';
import { reservationApi } from '../services/reservationApi';

// モック設定
vi.mock('../services/reservationApi');
const mockReservationApi = vi.mocked(reservationApi);

// テスト用ラッパー
function createWrapper() {
  const queryClient = new QueryClient({
    defaultOptions: {
      queries: { retry: false },
      mutations: { retry: false },
    },
  });
  
  return ({ children }: { children: React.ReactNode }) => (
    <QueryClientProvider client={queryClient}>
      {children}
    </QueryClientProvider>
  );
}

describe('useReservations', () => {
  beforeEach(() => {
    vi.clearAllMocks();
  });

  it('should fetch reservations successfully', async () => {
    const mockReservations = [
      { id: '1', roomName: 'Room A', status: 'CONFIRMED' },
      { id: '2', roomName: 'Room B', status: 'CONFIRMED' },
    ];
    
    mockReservationApi.getReservations.mockResolvedValue(mockReservations);

    const { result } = renderHook(() => useReservations(), {
      wrapper: createWrapper(),
    });

    await waitFor(() => {
      expect(result.current.loading).toBe(false);
    });

    expect(result.current.reservations).toEqual(mockReservations);
    expect(result.current.error).toBeNull();
  });

  it('should create reservation successfully', async () => {
    const newReservation = { id: '3', roomName: 'Room C', status: 'CONFIRMED' };
    mockReservationApi.createReservation.mockResolvedValue(newReservation);

    const { result } = renderHook(() => useReservations(), {
      wrapper: createWrapper(),
    });

    await act(async () => {
      result.current.createReservation.mutate({
        roomId: 'room-1',
        startTime: new Date(),
        endTime: new Date(),
        purpose: 'Meeting',
      });
    });

    expect(mockReservationApi.createReservation).toHaveBeenCalledWith({
      roomId: 'room-1',
      startTime: expect.any(Date),
      endTime: expect.any(Date),
      purpose: 'Meeting',
    });
  });
});
```

### 7.3 統合テスト例

```typescript
// features/reservation/components/ReservationList.test.tsx
import { render, screen, fireEvent, waitFor } from '@testing-library/react';
import { ReservationListContainer } from './ReservationListContainer';
import { setupMswServer } from '@/testing/mocks/server';

const server = setupMswServer();

beforeAll(() => server.listen());
afterEach(() => server.resetHandlers());
afterAll(() => server.close());

describe('ReservationListContainer', () => {
  it('should display reservations and handle cancellation', async () => {
    render(<ReservationListContainer />);

    // 予約一覧が表示されるのを待つ
    await waitFor(() => {
      expect(screen.getByText('Room A')).toBeInTheDocument();
      expect(screen.getByText('Room B')).toBeInTheDocument();
    });

    // キャンセルボタンをクリック
    const cancelButton = screen.getAllByText('キャンセル')[0];
    fireEvent.click(cancelButton);

    // キャンセル確認
    await waitFor(() => {
      expect(screen.getByText('予約がキャンセルされました')).toBeInTheDocument();
    });
  });
});
```

## 8. デプロイメント・ビルド設定

### 8.1 Vite設定

```typescript
// vite.config.ts
import { defineConfig } from 'vite';
import react from '@vitejs/plugin-react';
import path from 'path';

export default defineConfig({
  plugins: [react()],
  resolve: {
    alias: {
      '@': path.resolve(__dirname, './src'),
    },
  },
  build: {
    outDir: 'dist',
    sourcemap: true,
    rollupOptions: {
      output: {
        manualChunks: {
          vendor: ['react', 'react-dom'],
          router: ['react-router-dom'],
          ui: ['@headlessui/react', 'lucide-react'],
        },
      },
    },
  },
  server: {
    port: 3000,
    proxy: {
      '/api': {
        target: 'http://localhost:8080',
        changeOrigin: true,
      },
    },
  },
});
```

### 8.2 環境変数管理

```typescript
// config/env.ts
const env = {
  API_BASE_URL: import.meta.env.VITE_API_BASE_URL || 'http://localhost:8080/api',
  ENVIRONMENT: import.meta.env.VITE_ENVIRONMENT || 'development',
  VERSION: import.meta.env.VITE_APP_VERSION || '1.0.0',
} as const;

// バリデーション
const requiredEnvVars = ['VITE_API_BASE_URL'] as const;

for (const envVar of requiredEnvVars) {
  if (!import.meta.env[envVar]) {
    throw new Error(`Missing required environment variable: ${envVar}`);
  }
}

export { env };
```

## 9. セキュリティ対策

### 9.1 XSS対策

```typescript
// utils/security.ts
import DOMPurify from 'dompurify';

export function sanitizeHtml(html: string): string {
  return DOMPurify.sanitize(html);
}

export function escapeHtml(text: string): string {
  const div = document.createElement('div');
  div.textContent = text;
  return div.innerHTML;
}
```

### 9.2 CSP (Content Security Policy)

```html
<!-- index.html -->
<meta http-equiv="Content-Security-Policy" content="
  default-src 'self';
  script-src 'self' 'unsafe-inline';
  style-src 'self' 'unsafe-inline';
  img-src 'self' data: https:;
  font-src 'self';
  connect-src 'self' http://localhost:8080;
">
```

## 10. 運用・監視

### 10.1 エラー監視

```typescript
// lib/error-tracking.ts
class ErrorTracker {
  static captureException(error: Error, context?: Record<string, any>) {
    console.error('Application Error:', error, context);
    
    // 本番環境では外部エラートラッキングサービスに送信
    if (import.meta.env.PROD) {
      // Sentry.captureException(error, { extra: context });
    }
  }

  static captureMessage(message: string, level: 'info' | 'warning' | 'error' = 'info') {
    console[level]('Application Message:', message);
    
    if (import.meta.env.PROD) {
      // Sentry.captureMessage(message, level);
    }
  }
}

export { ErrorTracker };
```

### 10.2 パフォーマンス監視

```typescript
// hooks/usePerformanceMetrics.ts
import { useEffect } from 'react';

export function usePerformanceMetrics(componentName: string) {
  useEffect(() => {
    const startTime = performance.now();
    
    return () => {
      const endTime = performance.now();
      const renderTime = endTime - startTime;
      
      if (renderTime > 100) { // 100ms以上の場合は警告
        console.warn(`Slow render detected: ${componentName} took ${renderTime}ms`);
      }
      
      // メトリクス収集サービスに送信
      // analytics.track('component_render', {
      //   component: componentName,
      //   renderTime: renderTime,
      // });
    };
  }, [componentName]);
}
```

このフロントエンドアーキテクチャ設計により、保守しやすく、スケーラブルで、高性能なWebアプリケーションを構築できます。