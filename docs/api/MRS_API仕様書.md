# MRS (会議室予約システム) API仕様書

バージョン: v1.0  
最終更新: 2025-09-03  
対象: イテレーション3完了版  

## 概要

会議室予約システム（MRS）のREST API仕様書です。このAPIは認証、会議室管理、予約作成・取得・キャンセル機能を提供します。

### ベースURL
```
http://localhost:8080/api
```

### 認証方式
JWT（JSON Web Token）ベアラー認証

### 共通レスポンス形式

#### 成功時
```json
{
  "data": { ... },
  "timestamp": "2025-09-03T10:00:00.000Z"
}
```

#### エラー時
```json
{
  "error": {
    "code": "ERROR_CODE",
    "message": "エラーメッセージ",
    "details": "詳細情報"
  },
  "timestamp": "2025-09-03T10:00:00.000Z"
}
```

## エンドポイント仕様

### 1. 認証API

#### POST /api/auth/login
**概要**: ユーザーログイン

**リクエスト**:
```json
{
  "userId": "user001",
  "password": "password123"
}
```

**レスポンス**:
- **200 OK**: ログイン成功
```json
{
  "token": "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9...",
  "user": {
    "userId": "user001",
    "name": "山田太郎",
    "role": "USER"
  }
}
```

- **401 Unauthorized**: ログイン失敗
```json
{
  "error": {
    "code": "AUTHENTICATION_FAILED",
    "message": "認証に失敗しました"
  }
}
```

### 2. 会議室管理API

#### GET /api/rooms
**概要**: 予約可能会議室一覧取得

**パラメータ**:
- `date` (query, required): 対象日 (YYYY-MM-DD形式)

**ヘッダー**:
```
Authorization: Bearer {JWT_TOKEN}
```

**レスポンス**:
- **200 OK**: 取得成功
```json
[
  {
    "roomId": 1,
    "roomName": "会議室A",
    "reservableDate": "2025-09-04"
  },
  {
    "roomId": 2,
    "roomName": "会議室B",
    "reservableDate": "2025-09-04"
  }
]
```

- **401 Unauthorized**: 認証エラー

### 3. 予約管理API

#### POST /api/reservations
**概要**: 新規予約作成

**ヘッダー**:
```
Authorization: Bearer {JWT_TOKEN}
```

**リクエスト**:
```json
{
  "roomId": 1,
  "date": "2025-09-04",
  "startTime": "10:00",
  "endTime": "11:00"
}
```

**レスポンス**:
- **201 Created**: 予約作成成功
```json
{
  "reservationId": 123,
  "roomId": 1,
  "roomName": "会議室A",
  "userId": "user001",
  "userName": "山田太郎",
  "date": "2025-09-04",
  "startTime": "10:00",
  "endTime": "11:00",
  "createdAt": "2025-09-03T10:00:00.000Z"
}
```

- **400 Bad Request**: バリデーションエラー
```json
{
  "error": {
    "code": "VALIDATION_ERROR",
    "message": "入力値が不正です",
    "details": {
      "startTime": "開始時刻は必須です",
      "endTime": "終了時刻は開始時刻より後である必要があります"
    }
  }
}
```

- **409 Conflict**: 重複予約エラー
```json
{
  "error": {
    "code": "RESERVATION_CONFLICT",
    "message": "指定された時間帯は既に予約されています",
    "details": "2025-09-04 10:00-11:00は利用できません"
  }
}
```

#### GET /api/reservations
**概要**: 予約一覧取得

**パラメータ**:
- `date` (query, optional): 対象日 (YYYY-MM-DD形式)
- `userId` (query, optional): ユーザーID
- `roomId` (query, optional): 会議室ID

**ヘッダー**:
```
Authorization: Bearer {JWT_TOKEN}
```

**レスポンス**:
- **200 OK**: 取得成功
```json
[
  {
    "reservationId": 123,
    "roomId": 1,
    "roomName": "会議室A",
    "userId": "user001",
    "userName": "山田太郎",
    "date": "2025-09-04",
    "startTime": "10:00",
    "endTime": "11:00",
    "createdAt": "2025-09-03T10:00:00.000Z"
  }
]
```

#### DELETE /api/reservations/{reservationId}
**概要**: 予約キャンセル

**パラメータ**:
- `reservationId` (path, required): 予約ID

**ヘッダー**:
```
Authorization: Bearer {JWT_TOKEN}
```

**レスポンス**:
- **204 No Content**: キャンセル成功

- **403 Forbidden**: 権限不足
```json
{
  "error": {
    "code": "ACCESS_DENIED",
    "message": "この予約をキャンセルする権限がありません"
  }
}
```

- **404 Not Found**: 予約が見つからない
```json
{
  "error": {
    "code": "RESERVATION_NOT_FOUND",
    "message": "指定された予約が見つかりません",
    "details": "reservationId: 123"
  }
}
```

## 権限制御

### ロール定義
- **USER**: 一般ユーザー
  - 自分の予約作成・参照・キャンセル
- **ADMIN**: 管理者
  - 全ての予約の参照・キャンセル
  - ユーザー管理（予定）

### 権限マトリクス

| 操作 | USER | ADMIN |
|------|------|--------|
| ログイン | ✅ | ✅ |
| 会議室一覧取得 | ✅ | ✅ |
| 予約作成 | ✅ | ✅ |
| 自分の予約参照 | ✅ | ✅ |
| 他者の予約参照 | ❌ | ✅ |
| 自分の予約キャンセル | ✅ | ✅ |
| 他者の予約キャンセル | ❌ | ✅ |

## エラーコード一覧

| コード | HTTPステータス | 説明 |
|--------|----------------|------|
| AUTHENTICATION_FAILED | 401 | 認証失敗 |
| ACCESS_DENIED | 403 | アクセス権限不足 |
| VALIDATION_ERROR | 400 | 入力値検証エラー |
| RESERVATION_CONFLICT | 409 | 予約競合エラー |
| RESERVATION_NOT_FOUND | 404 | 予約が見つからない |
| ROOM_NOT_FOUND | 404 | 会議室が見つからない |
| INTERNAL_SERVER_ERROR | 500 | サーバー内部エラー |

## パフォーマンス要件

| エンドポイント | 応答時間目標 | 備考 |
|----------------|-------------|------|
| POST /api/auth/login | 1秒以内 | N001-01準拠 |
| GET /api/rooms | 2秒以内 | N001-01準拠 |
| POST /api/reservations | 3秒以内 | N001-01準拠 |
| GET /api/reservations | 2秒以内 | N001-01準拠 |
| DELETE /api/reservations/{id} | 1秒以内 | N001-01準拠 |

## セキュリティ

### JWT設定
- **アルゴリズム**: HS256
- **有効期限**: 24時間
- **リフレッシュ**: 自動延長なし（再ログイン必要）

### 並行処理制御
- **悲観ロック**: 予約作成時にreservable_roomテーブルをFOR UPDATE
- **重複検出**: `target.endTime > this.startTime && this.endTime > target.startTime`

### 負荷制限
- **同時接続数**: 100ユーザー (N001-02準拠)
- **レート制限**: 未実装（今後検討）

## バージョン履歴

| バージョン | 日付 | 変更内容 |
|-----------|------|----------|
| v1.0 | 2025-09-03 | 初版リリース（イテレーション3完了版）|
|  | | - 認証・会議室・予約CRUD機能 |
|  | | - キャンセル機能追加 |
|  | | - 権限制御実装 |
|  | | - 悲観ロック・重複検出対応 |

## 開発・テスト情報

### テスト用アカウント
- **一般ユーザー**: user001 / password
- **管理者**: admin / admin123

### モックデータ
- **会議室**: 1〜3 (会議室A、B、C)
- **テストデータ**: H2インメモリDBで自動生成

### 関連ドキュメント
- [要件仕様書](../requirements/仕様.md)
- [イテレーション計画3](../development/イテレーション計画3.md)
- [負荷テストガイド](../test/負荷テストガイド.md)
- [品質ゲートレポート](../development/品質ゲートレポート_I3.md)