# IDE データベース接続ガイド

## 接続情報

```
Type: PostgreSQL
Host: localhost
Port: 5432
Database: sales_management
User: postgres
Password: password
```

## IntelliJ IDEA / DataGrip 設定

### 1. Database Tool Window を開く

- `View` → `Tool Windows` → `Database`
- または `⌘ + Shift + A` → "Database" と入力

### 2. 新規データソース追加

1. `+` ボタンをクリック
2. `Data Source` → `PostgreSQL` を選択

### 3. 接続設定

**General タブ:**

| フィールド | 値 |
|-----------|-----|
| Name | Sales Management (Go) |
| Host | localhost |
| Port | 5432 |
| Database | sales_management |
| User | postgres |
| Password | password |
| Save | ✅ password |

**または URL を直接入力:**
```
jdbc:postgresql://localhost:5432/sales_management
```

### 4. ドライバ設定

- PostgreSQL ドライバが未インストールの場合、"Download missing driver files" をクリック
- 最新の PostgreSQL Driver を選択

### 5. 詳細設定（必要な場合）

**SSH/SSL タブ:**
- Use SSL: `No` または `disable`

**Advanced タブ:**
追加のプロパティ（必要に応じて）:
```
ssl=false
sslmode=disable
```

### 6. 接続テスト

1. `Test Connection` ボタンをクリック
2. 成功メッセージを確認: `Succeeded`
3. `OK` をクリックして保存

## よくあるエラーと解決方法

### Error: "Driver not found"

**解決方法:**
- Database Tool Window で PostgreSQL ドライバをダウンロード
- または手動で最新の JDBC ドライバを追加

### Error: "Authentication failed"

**原因:** パスワードが間違っている可能性

**確認方法:**
```bash
docker exec sales-management-postgres psql -U postgres -c "\du"
```

**解決方法:**
- パスワードを再確認: `password`
- .env ファイルの POSTGRES_PASSWORD を確認

### Error: "Connection refused"

**原因:** PostgreSQL コンテナが起動していない

**確認方法:**
```bash
docker compose ps postgres
```

**解決方法:**
```bash
docker compose up -d postgres
```

### Error: "SSL connection required"

**原因:** SSL が必須になっている

**解決方法:**
- Advanced タブで `sslmode=disable` を追加
- または URL に `?sslmode=disable` を追加

## 接続確認コマンド

### コンテナ状態確認
```bash
docker compose ps postgres
```

### ポート確認
```bash
nc -zv localhost 5432
```

### データベース一覧
```bash
docker exec sales-management-postgres psql -U postgres -l
```

### 接続テスト
```bash
docker exec sales-management-postgres psql -U postgres -d sales_management -c "SELECT version();"
```

## 代替接続方法

### 1. Adminer（Web UI）

http://localhost:8080 にアクセス

```
System: PostgreSQL
Server: postgres (コンテナ名)
Username: postgres
Password: password
Database: sales_management
```

### 2. コマンドライン（psql）

```bash
docker exec -it sales-management-postgres psql -U postgres -d sales_management
```

### 3. DBeaver

1. 新規接続 → PostgreSQL
2. 上記と同じ接続情報を入力
3. Test Connection → Finish

## スクリーンショット用の設定例

```yaml
データベース接続設定:
  タイプ: PostgreSQL
  接続方法: Standard
  ホスト名: localhost
  ポート番号: 5432
  データベース名: sales_management
  ユーザー名: postgres
  パスワード: password
  SSL: 無効

JDBC URL:
  jdbc:postgresql://localhost:5432/sales_management?sslmode=disable
```

## トラブルシューティングチェックリスト

- [ ] Docker Desktop が起動している
- [ ] PostgreSQL コンテナが起動している (`docker compose ps`)
- [ ] ポート 5432 が開いている (`nc -zv localhost 5432`)
- [ ] データベース `sales_management` が存在する
- [ ] ユーザー名とパスワードが正しい (`postgres` / `password`)
- [ ] PostgreSQL ドライバがダウンロード済み
- [ ] SSL 設定が無効になっている
- [ ] ファイアウォールがポートをブロックしていない
