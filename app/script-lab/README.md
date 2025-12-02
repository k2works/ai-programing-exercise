# Excel Script-Lab クライアント

Excel Script-Labを使用してDDD書籍管理システムのAPIを呼び出すクライアントです。

## セットアップ

### 1. APIサーバーの起動

```bash
# データベースを起動
cd app
docker compose up -d

# マイグレーション実行
npm run migrate:dev

# APIサーバーを起動
npm run dev
```

APIサーバーは http://localhost:3000 で起動します。

### 2. Script-Labのインストール

1. Excelを開く
2. 「挿入」タブから「アドインを入手」をクリック
3. 「Script Lab」を検索してインストール

### 3. スニペットのインポート

1. Excelで Script Lab を開く
2. 「Import」ボタンをクリック
3. `book-registration-client.yaml` ファイルの内容をコピー＆ペースト
4. 「Import」をクリック

## 使い方

### 書籍の登録

1. ISBN、タイトル、価格を入力
2. 「登録」ボタンをクリック
3. 成功メッセージが表示されます

例:
- ISBN: `9784167158057`
- タイトル: `吾輩は猫である`
- 価格: `770`

### 書籍の取得

1. ISBNを入力
2. 「取得」ボタンをクリック
3. 書籍情報が表示されます

## API エンドポイント

### POST /book
書籍を登録

**リクエスト:**
```json
{
  "isbn": "9784167158057",
  "title": "吾輩は猫である",
  "priceAmount": 770
}
```

**レスポンス:**
```json
{
  "message": "success"
}
```

### GET /book/:isbn
書籍を取得

**レスポンス:**
```json
{
  "bookId": "9784167158057",
  "title": "吾輩は猫である",
  "price": 770,
  "stockId": "xxx",
  "quantityAvailable": 0,
  "status": "在庫切れ"
}
```

## トラブルシューティング

### CORSエラーが発生する場合

APIサーバーにCORS設定が追加されているか確認してください。

### 通信エラーが発生する場合

- APIサーバーが起動しているか確認
- データベースが起動しているか確認
- ブラウザのコンソールでエラーメッセージを確認
