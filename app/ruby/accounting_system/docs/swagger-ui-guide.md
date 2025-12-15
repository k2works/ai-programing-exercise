# Swagger UI 使用ガイド

## 概要

Accounting System API は OpenAPI 3.0 仕様に基づいた API ドキュメントを提供しています。
Swagger UI を使用して、ブラウザから API の仕様を確認し、実際にリクエストを送信してテストすることができます。

## Swagger UI へのアクセス

### 1. Rails サーバーの起動

```bash
cd /Users/k2works/IdeaProjects/ai-programing-exercise/app/ruby/accounting_system
rails server
```

### 2. ブラウザでアクセス

以下の URL にアクセスします：

```
http://localhost:3000/api-docs
```

## トラブルシューティング

### "Failed to fetch" エラーが表示される場合

#### 原因 1: サーバーが起動していない

```bash
# サーバーが起動しているか確認
lsof -ti:3000

# 起動していない場合は起動
rails server
```

#### 原因 2: Swagger ドキュメントが生成されていない

```bash
# Swagger ドキュメントを再生成
RAILS_ENV=test bundle exec rake rswag:specs:swaggerize

# 生成されたファイルを確認
ls -la swagger/v1/swagger.yaml
```

#### 原因 3: ブラウザのキャッシュ

1. ブラウザのデベロッパーツールを開く（F12）
2. ネットワークタブを確認
3. ハードリロード（Ctrl+Shift+R または Cmd+Shift+R）
4. キャッシュをクリアしてリロード

### エンドポイントの確認

```bash
# Swagger YAML が取得できるか確認
curl http://localhost:3000/api-docs/v1/swagger.yaml

# Swagger UI のページが取得できるか確認
curl http://localhost:3000/api-docs
```

## API エンドポイント一覧

### 勘定科目 API

- `GET /api/v1/accounts` - 一覧取得
- `POST /api/v1/accounts` - 新規作成
- `GET /api/v1/accounts/:code` - 詳細取得
- `PUT /api/v1/accounts/:code` - 更新
- `DELETE /api/v1/accounts/:code` - 削除

### 仕訳 API

- `GET /api/v1/journal-entries` - 一覧取得
- `POST /api/v1/journal-entries` - 新規作成
- `GET /api/v1/journal-entries/:id` - 詳細取得
- `DELETE /api/v1/journal-entries/:id` - 削除

### 財務諸表 API

- `GET /api/v1/financial-statements/balance-sheet` - 貸借対照表取得
- `GET /api/v1/financial-statements/income-statement` - 損益計算書取得
- `GET /api/v1/financial-statements/ratios` - 財務指標取得

## Swagger UI の使い方

### 1. API エンドポイントの確認

Swagger UI のページを開くと、すべての API エンドポイントが一覧表示されます。
各エンドポイントをクリックすると、詳細情報が表示されます。

### 2. リクエストの送信

1. エンドポイントを展開
2. "Try it out" ボタンをクリック
3. 必要なパラメータを入力
4. "Execute" ボタンをクリック
5. レスポンスが表示される

### 3. スキーマの確認

各エンドポイントの "Responses" セクションで、レスポンスのスキーマを確認できます。

## Swagger ドキュメントの更新

API に変更を加えた場合は、以下のコマンドで Swagger ドキュメントを再生成します：

```bash
RAILS_ENV=test bundle exec rake rswag:specs:swaggerize
```

生成されたドキュメントは `swagger/v1/swagger.yaml` に保存されます。

## 開発環境での注意事項

### CORS について

開発環境では、Swagger UI と API は同一オリジン（localhost:3000）で動作するため、CORS の設定は不要です。

### Production 環境

Production 環境で Swagger UI を公開する場合は、セキュリティに注意してください：

1. Basic 認証の有効化を検討
2. HTTPS の使用
3. IP アドレス制限の検討

```ruby
# config/initializers/rswag_ui.rb
Rswag::Ui.configure do |c|
  c.basic_auth_enabled = true
  c.basic_auth_credentials 'username', 'password'
end
```

## 参考リンク

- [Swagger UI 公式ドキュメント](https://swagger.io/tools/swagger-ui/)
- [OpenAPI Specification](https://swagger.io/specification/)
- [rswag GitHub](https://github.com/rswag/rswag)
