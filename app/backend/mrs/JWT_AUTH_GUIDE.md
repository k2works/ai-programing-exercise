# JWT認証の具体的な使い方ガイド

## 🔐 テストユーザー情報
現在、開発環境には以下のテストユーザーが登録されています：

| ユーザーID | パスワード | 権限 |
|-----------|----------|------|
| user1     | demo     | 一般ユーザー |
| admin     | admin    | 管理者 |

## 📝 JWT認証の流れ

```
1. ログイン → JWTトークン取得
2. トークンをヘッダーに付けてAPIリクエスト
3. トークンが有効な間はAPIアクセス可能（15分間）
4. トークンが切れたらリフレッシュまたは再ログイン
```

## 🚀 方法1: curlコマンドで認証する

### ステップ1: ログインしてトークンを取得

```bash
# Windowsコマンドプロンプト
curl -X POST http://localhost:8080/api/auth/login ^
  -H "Content-Type: application/json" ^
  -d "{\"username\":\"user1\",\"password\":\"demo\"}"

# PowerShell
Invoke-RestMethod -Uri "http://localhost:8080/api/auth/login" `
  -Method POST `
  -ContentType "application/json" `
  -Body '{"username":"user1","password":"demo"}'

# Git Bash / Linux / Mac
curl -X POST http://localhost:8080/api/auth/login \
  -H "Content-Type: application/json" \
  -d '{"username":"user1","password":"demo"}'
```

### レスポンス例
```json
{
  "accessToken": "eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJ1c2VyMSIsImlhdCI6MTcwOTI3...",
  "refreshToken": "eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJ1c2VyMSIsImlhdCI6MTcwOTI3...",
  "tokenType": "Bearer",
  "expiresIn": 900
}
```

### ステップ2: トークンを使ってAPIにアクセス

**重要**: `accessToken`の値をコピーして使います

```bash
# Windowsコマンドプロンプト
curl -X GET http://localhost:8080/api/rooms ^
  -H "Authorization: Bearer eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJ1c2VyMSIsImlhdCI6MTcwOTI3..."

# PowerShell
Invoke-RestMethod -Uri "http://localhost:8080/api/rooms" `
  -Method GET `
  -Headers @{Authorization="Bearer eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJ1c2VyMSIsImlhdCI6MTcwOTI3..."}

# Git Bash / Linux / Mac
curl -X GET http://localhost:8080/api/rooms \
  -H "Authorization: Bearer eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJ1c2VyMSIsImlhdCI6MTcwOTI3..."
```

## 🎯 方法2: Swagger UIで認証する（推奨）

### ステップ1: Swagger UIを開く
1. アプリケーションを起動
   ```bash
   cd app/backend/mrs
   ./gradlew bootRun
   ```
2. ブラウザで http://localhost:8080/swagger-ui.html にアクセス

### ステップ2: ログインAPIでトークンを取得

1. **POST /api/auth/login** を探してクリック
2. **Try it out** ボタンをクリック
3. Request bodyに以下を入力:
   ```json
   {
     "userId": "user1",
     "password": "demo"
   }
   ```
4. **Execute** ボタンをクリック
5. Response bodyから `accessToken` の値をコピー（ダブルクォートは除く）

### ステップ3: Swagger UIで認証設定

1. ページ上部の **Authorize** ボタン（鍵アイコン）をクリック
2. 表示されたダイアログで:
   - Value欄に **トークンの値のみ** を入力（Bearerプレフィックスは自動で付きます）
   - **重要**: `Bearer` は入力不要！トークンの値だけを貼り付け
   - ✅ 正しい例: `eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJ1c2VyMSIsImlhdCI6MTcwOTI3...`
   - ❌ 間違い例: `Bearer eyJhbGciOiJIUzI1NiJ9...`
3. **Authorize** ボタンをクリック
4. **Close** ボタンでダイアログを閉じる

### ステップ4: 認証が必要なAPIを実行

1. **GET /api/rooms** を探してクリック
2. **Try it out** → **Execute**
3. 認証成功していれば会議室一覧が返ってきます

## ⚠️ よくある間違い

### curlコマンドの場合
#### ❌ 間違い1: Bearer を付け忘れる
```
Authorization: eyJhbGciOiJIUzI1NiJ9...  ← NG
Authorization: Bearer eyJhbGciOiJIUzI1NiJ9...  ← OK
```

#### ❌ 間違い2: ダブルクォートも含めてコピー
```
Authorization: Bearer "eyJhbGciOiJIUzI1NiJ9..."  ← NG
Authorization: Bearer eyJhbGciOiJIUzI1NiJ9...    ← OK
```

#### ❌ 間違い3: スペースを忘れる
```
Authorization: BearereyJhbGciOiJIUzI1NiJ9...  ← NG
Authorization: Bearer eyJhbGciOiJIUzI1NiJ9... ← OK
```

### Swagger UIの場合
#### ❌ 間違い1: Bearerを手動で付ける
```
Bearer eyJhbGciOiJIUzI1NiJ9...  ← NG（自動で付くので不要）
eyJhbGciOiJIUzI1NiJ9...        ← OK（トークンの値のみ）
```

#### ❌ 間違い2: ダブルクォートも含める
```
"eyJhbGciOiJIUzI1NiJ9..."  ← NG
eyJhbGciOiJIUzI1NiJ9...    ← OK
```

## 🔄 トークンのリフレッシュ

アクセストークンは15分で期限切れになります。リフレッシュする方法：

```bash
# refreshTokenを使って新しいaccessTokenを取得
curl -X POST http://localhost:8080/api/auth/refresh \
  -H "Content-Type: application/json" \
  -d '{"refreshToken":"eyJhbGciOiJIUzI1NiJ9..."}'
```

## 📊 トークンの有効期限

| トークン種別 | 有効期限 | 用途 |
|------------|---------|------|
| accessToken | 15分 | API認証用 |
| refreshToken | 30日 | 新しいaccessToken取得用 |

## 🧪 動作確認手順

1. まず認証なしでAPIにアクセスしてみる → 401 Unauthorized エラーになることを確認
2. ログインしてトークン取得
3. トークンを付けてAPIアクセス → 200 OK で成功
4. 15分待ってアクセス → 401エラー
5. リフレッシュトークンで更新 → 新しいトークンで成功

## 💡 Tips

- **Swagger UI推奨**: ブラウザから簡単にAPIテストできるのでSwagger UIの使用を推奨
- Postmanを使う場合は従来通り `Bearer {トークン}` の形式で設定
- Chrome拡張の「ModHeader」を使えばブラウザでも認証ヘッダーを設定できます
- 開発時は `JWT_SECRET` 環境変数を固定値にすると、サーバー再起動してもトークンが有効のまま使えます

## 🔑 認証方式の違い

| ツール | Bearer プレフィックス | 入力例 |
|--------|---------------------|--------|
| curl | **必要** | `Bearer eyJhbGci...` |
| Postman | **必要** | `Bearer eyJhbGci...` |
| **Swagger UI** | **不要** | `eyJhbGci...` |
| ModHeader | **必要** | `Bearer eyJhbGci...` |

## 🚨 重要な変更点

**2025-08-29更新**: Swagger UIでの認証方式を修正しました
- **旧**: `Bearer {トークン}` を手動入力
- **新**: トークンの値のみ入力（Bearerは自動付与）

これにより、Swagger UIでの認証がより簡単になりました！