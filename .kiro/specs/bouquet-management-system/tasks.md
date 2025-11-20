# 実装計画

## プロジェクトセットアップ

- [x] 1. プロジェクト構造とツールチェーンのセットアップ
  - プロジェクトは appディレクトリ直下に作成する
  - モノレポ構造（frontend/backend）の作成
  - TypeScript、ESLint、Prettier、Vitestの設定
  - Gulpタスクランナーの設定（自動lint、format、test）
  - dependency-cruiserによる循環参照検知の設定
  - _要件: 全体_

- [x] 2. バックエンド基盤のセットアップ
  - Fastifyサーバーの初期化
  - Prisma ORMのセットアップとPostgreSQL接続
  - JWT認証プラグインの設定
  - Zodバリデーションの統合
  - OpenAPI/Swagger UIの設定
  - _要件: 全体_

- [x] 3. フロントエンド基盤のセットアップ
  - Vite + Reactプロジェクトの初期化
  - React Router v6のセットアップ
  - Material-UI/Tailwind CSSの設定
  - Orval設定（OpenAPI → TypeScript型生成）
  - Axiosクライアントの設定
  - _要件: 全体_

- [x] 4. 開発環境のDockerセットアップ
  - docker-compose.ymlの作成（PostgreSQL、バックエンド、フロントエンド）
  - 環境変数管理（.env.example）
  - Testcontainersの設定
  - _要件: 全体_

## データモデルとスキーマ

- [x] 5. Prismaスキーマの定義
  - 認証スキーマ（User）
  - 注文スキーマ（Product, Customer, Order, ReceivedOrder, Shipment, Sales, Return）
  - 在庫スキーマ（Item, Supplier, Inventory, PlacementOrder, PlacementOrderLine, Arrival, ArrivalLine, Purchase）
  - リレーションシップの定義
  - _要件: 全体_

- [ ] 6. データベースマイグレーションの作成と実行
  - 初期マイグレーションの生成
  - シードデータの作成
  - マイグレーション実行の確認
  - _要件: 全体_

## ドメインモデルと値オブジェクト

- [ ] 7. 値オブジェクトの実装
  - UserId, UserName, Password, Email, RoleName
  - ProductId, ProductCode, ProductName, Price, SalesStatus
  - OrderId, OrderDate, DeliveryDate, DeliveryAddress, OrderStatus
  - ItemId, ItemCode, QualityDays, LeadTime, LotNumber, Quantity
  - CustomerId, SupplierId
  - _要件: 1, 2, 3, 6, 10, 18, 19_

- [ ]* 7.1 値オブジェクトのプロパティベーステスト
  - **Property 1: 値オブジェクトの等価性**
  - **検証: 要件 全体**

## 認証コンテキスト

- [ ] 8. ユーザー集約の実装
  - Userエンティティの実装
  - パスワードハッシュ化（bcrypt）
  - 認証メソッド（authenticate）
  - ユーザー状態管理（activate/deactivate）
  - _要件: 1.1, 1.2, 1.5, 2.1, 2.3, 2.4_

- [ ]* 8.1 ユーザー認証のプロパティベーステスト
  - **Property 2: 有効な認証情報での認証成功**
  - **検証: 要件 1.1**

- [ ]* 8.2 無効な認証情報のプロパティベーステスト
  - **Property 3: 無効な認証情報での認証失敗**
  - **検証: 要件 1.2**

- [ ] 9. 認証サービスの実装
  - JWT生成とバリデーション
  - セッション管理（30分タイムアウト）
  - ログアウト処理
  - _要件: 1.1, 1.2, 1.3, 1.5_

- [ ]* 9.1 セッションタイムアウトのプロパティベーステスト
  - **Property 4: 30分非アクティブでセッション終了**
  - **検証: 要件 1.3**

- [ ] 10. 認証APIエンドポイントの実装
  - POST /api/auth/login
  - POST /api/auth/logout
  - GET /api/auth/me
  - _要件: 1.1, 1.5_

## ユーザー管理コンテキスト

- [ ] 11. ユーザーリポジトリの実装
  - findById, findByEmail
  - save, delete
  - Prismaとの統合
  - _要件: 2.1, 2.2, 2.5_

- [ ] 12. ユーザー管理サービスの実装
  - ユーザー登録（registerUser）
  - ユーザー更新（updateUser）
  - ユーザー無効化/再有効化（deactivate/reactivate）
  - ユーザー削除（deleteUser）
  - 監査証跡の記録
  - _要件: 2.1, 2.2, 2.3, 2.4, 2.5_

- [ ]* 12.1 ユーザー管理のプロパティベーステスト
  - **Property 5: ユーザー作成後の取得一貫性**
  - **検証: 要件 2.1**

- [ ] 13. ユーザー管理APIエンドポイントの実装
  - POST /api/users
  - PUT /api/users/:id
  - DELETE /api/users/:id
  - PATCH /api/users/:id/deactivate
  - PATCH /api/users/:id/reactivate
  - _要件: 2.1, 2.2, 2.3, 2.4, 2.5_

## 商品管理コンテキスト

- [ ] 14. 商品集約の実装
  - Productエンティティ
  - ProductCompositionエンティティ
  - 販売状態管理（canOrder, stopSales, resumeSales, endSales）
  - 必要単品の計算（getRequiredItems）
  - _要件: 3.1, 3.2, 3.3, 3.4, 3.5, 4.1, 4.2, 4.3, 4.4, 4.5_

- [ ]* 14.1 商品販売状態のプロパティベーステスト
  - **Property 6: 販売中の商品のみ注文可能**
  - **検証: 要件 4.4**

- [ ]* 14.2 商品販売終了のプロパティベーステスト
  - **Property 7: 保留中注文がある商品は販売終了不可**
  - **検証: 要件 4.5**

- [ ] 15. 商品リポジトリの実装
  - findById, findByCode, findOnSale
  - save
  - Prismaとの統合
  - _要件: 3.1, 3.2, 3.3, 3.5_

- [ ] 16. 商品管理サービスの実装
  - 商品登録（registerProduct）
  - 商品更新（updateProduct）
  - 単品関連付け（associateItems）
  - 販売制御（stopSales, resumeSales, endSales）
  - _要件: 3.1, 3.2, 3.3, 4.1, 4.2, 4.3_

- [ ] 17. 商品管理APIエンドポイントの実装
  - POST /api/products
  - PUT /api/products/:id
  - GET /api/products/:id
  - GET /api/products
  - PATCH /api/products/:id/stop-sales
  - PATCH /api/products/:id/resume-sales
  - PATCH /api/products/:id/end-sales
  - _要件: 3.1, 3.2, 3.3, 3.5, 4.1, 4.2, 4.3_

## 注文管理コンテキスト

- [ ] 18. 注文集約の実装
  - Orderエンティティ
  - 注文作成バリデーション（配送日チェック）
  - 出荷日計算（getShipmentDate）
  - 注文変更（changeDeliveryDate）
  - 注文キャンセル（cancel）
  - _要件: 6.1, 6.2, 6.3, 6.4, 6.5, 8.1, 8.2, 8.3, 8.4, 8.5, 9.1, 9.2, 9.3, 9.4, 9.5_

- [ ]* 18.1 注文配送日バリデーションのプロパティベーステスト
  - **Property 8: 配送日は注文日の2日後以降**
  - **検証: 要件 6.4**

- [ ]* 18.2 出荷済み注文の変更不可のプロパティベーステスト
  - **Property 9: 出荷済み注文は変更・キャンセル不可**
  - **検証: 要件 8.5, 9.5**

- [ ] 19. カート管理の実装
  - カートへの商品追加
  - カートからの商品削除
  - カート合計金額計算
  - カートクリア
  - _要件: 5.3, 5.4, 5.5, 6.5_

- [ ]* 19.1 カート合計金額のプロパティベーステスト
  - **Property 10: カート合計金額のリアルタイム更新**
  - **検証: 要件 5.5**

- [ ] 20. 注文リポジトリの実装
  - findById, findByCustomerId, findPendingOrders
  - save
  - Prismaとの統合
  - _要件: 6.1, 7.1, 8.1, 9.1_

- [ ] 21. 注文サービスの実装
  - 注文配置（placeOrder）
  - 注文変更（modifyOrder）
  - 注文キャンセル（cancelOrder）
  - 注文確認通知
  - _要件: 6.1, 6.2, 6.3, 6.4, 6.5, 8.1, 8.2, 8.3, 8.4, 9.1, 9.2, 9.3, 9.4, 9.5_

- [ ] 22. 注文APIエンドポイントの実装（得意先向け）
  - POST /api/orders
  - PUT /api/orders/:id
  - DELETE /api/orders/:id
  - GET /api/orders
  - GET /api/orders/:id
  - _要件: 6.1, 8.1, 9.1_

## 受注処理コンテキスト

- [ ] 23. 在庫引当サービスの実装
  - 在庫引当ロジック（allocate）
  - FIFO（先入先出）アルゴリズム
  - 在庫不足チェック
  - 引当結果の返却
  - _要件: 7.2, 7.3, 7.4_

- [ ]* 23.1 在庫引当のプロパティベーステスト
  - **Property 11: 十分な在庫がある場合の引当成功**
  - **検証: 要件 7.3**

- [ ]* 23.2 在庫不足のプロパティベーステスト
  - **Property 12: 在庫不足時の引当失敗**
  - **検証: 要件 7.4**

- [ ] 24. 受注サービスの実装
  - 受注確認（confirmOrder）
  - 在庫引当の実行
  - 出荷日計算
  - 注文確認通知の送信
  - _要件: 7.1, 7.2, 7.3, 7.4, 7.5_

- [ ] 25. 受注APIエンドポイントの実装（スタッフ向け）
  - GET /api/staff/orders/pending
  - POST /api/staff/orders/:id/confirm
  - GET /api/staff/orders/:id
  - _要件: 7.1, 7.2_

## 在庫管理コンテキスト

- [ ] 26. 単品集約の実装
  - Itemエンティティ
  - 有効期限チェック（isExpired）
  - 最小発注日計算（getMinimumOrderDate）
  - _要件: 10.1, 10.3, 10.5, 11.3_

- [ ] 27. 在庫ロット集約の実装
  - InventoryLotエンティティ
  - 利用可能数量計算（getAvailableQuantity）
  - 在庫引当（allocate）
  - 有効期限チェック（isExpired）
  - _要件: 11.1, 11.2, 11.3, 11.4, 11.5_

- [ ]* 27.1 在庫ロット引当のプロパティベーステスト
  - **Property 13: 利用可能数量を超える引当は失敗**
  - **検証: 要件 11.2**

- [ ]* 27.2 有効期限切れ在庫のプロパティベーステスト
  - **Property 14: 有効期限切れロットは利用不可**
  - **検証: 要件 11.3**

- [ ] 28. 在庫リポジトリの実装
  - findByItemId, findAvailableLots
  - save
  - Prismaとの統合
  - _要件: 11.1, 11.2, 11.5_

- [ ] 29. 在庫推移計算サービスの実装
  - 日次在庫予測（calculate）
  - 現在在庫の計算
  - 予定入荷の計算
  - 引当済数量の計算
  - 不足の強調表示
  - _要件: 12.1, 12.2, 12.3, 12.4, 12.5_

- [ ]* 29.1 在庫推移計算のプロパティベーステスト
  - **Property 15: 在庫推移の一貫性**
  - **検証: 要件 12.1, 12.2, 12.3**

- [ ] 30. 単品・仕入先管理サービスの実装
  - 単品登録（registerItem）
  - 仕入先登録（registerSupplier）
  - 単品更新（updateItem）
  - 参照整合性の維持
  - _要件: 10.1, 10.2, 10.3, 10.4, 10.5_

- [ ] 31. 在庫管理APIエンドポイントの実装
  - POST /api/items
  - PUT /api/items/:id
  - GET /api/items/:id
  - POST /api/suppliers
  - PUT /api/suppliers/:id
  - GET /api/inventory/:itemId
  - GET /api/inventory/:itemId/projection
  - _要件: 10.1, 10.2, 10.3, 10.5, 11.5, 12.1_

## 発注管理コンテキスト

- [ ] 32. 発注集約の実装
  - PlacementOrderエンティティ
  - PlacementOrderLineエンティティ
  - リードタイムバリデーション
  - 購入単位数量チェック
  - 発注キャンセル（cancel）
  - _要件: 13.1, 13.2, 13.3, 13.4, 13.5, 9.3_

- [ ]* 32.1 発注リードタイムのプロパティベーステスト
  - **Property 16: 配送日はリードタイム日数後以降**
  - **検証: 要件 13.3**

- [ ]* 32.2 購入単位数量のプロパティベーステスト
  - **Property 17: 発注数量は購入単位の倍数**
  - **検証: 要件 13.2**

- [ ] 33. 発注リポジトリの実装
  - findById, findBySupplierId
  - save
  - Prismaとの統合
  - _要件: 13.1, 9.3_

- [ ] 34. 発注サービスの実装
  - 発注作成（createPlacementOrder）
  - 発注キャンセル（cancelPlacementOrder）
  - 在庫推移への反映
  - _要件: 13.1, 13.2, 13.3, 13.4, 13.5, 9.3, 9.4_

- [ ] 35. 入荷・検収サービスの実装
  - 入荷記録（receiveItems）
  - 検収処理（inspectItems）
  - 在庫への追加
  - 返品処理
  - 仕入レコード作成
  - _要件: 14.1, 14.2, 14.3, 14.4, 14.5_

- [ ]* 35.1 入荷検収のプロパティベーステスト
  - **Property 18: 受入時の在庫追加**
  - **検証: 要件 14.3**

- [ ] 36. 発注管理APIエンドポイントの実装
  - POST /api/placement-orders
  - DELETE /api/placement-orders/:id
  - POST /api/arrivals
  - POST /api/arrivals/:id/inspect
  - _要件: 13.1, 9.3, 14.1, 14.2_

## 出荷管理コンテキスト

- [ ] 37. 出荷サービスの実装
  - ピッキングリスト生成（generatePickingList）
  - ピッキング確認（confirmPicking）
  - 商品組立指示書生成
  - 在庫数量減少
  - _要件: 15.1, 15.2, 15.3, 15.4, 15.5_

- [ ]* 37.1 ピッキングのプロパティベーステスト
  - **Property 19: ピッキング確認時の在庫減少**
  - **検証: 要件 15.3**

- [ ] 38. 出荷実行サービスの実装
  - 出荷レコード作成（executeShipment）
  - 売上レコード作成
  - 出荷通知送信
  - 注文履行済みマーク
  - 出荷日バリデーション
  - _要件: 16.1, 16.2, 16.3, 16.4, 16.5_

- [ ]* 38.1 出荷実行のプロパティベーステスト
  - **Property 20: 出荷時の売上レコード作成**
  - **検証: 要件 16.2**

- [ ] 39. 返品処理サービスの実装
  - 返品レコード作成
  - 売上取引の取消
  - 会計レコード更新
  - 返品確認送信
  - 30日制限チェック
  - _要件: 17.1, 17.2, 17.3, 17.4, 17.5_

- [ ]* 39.1 返品期限のプロパティベーステスト
  - **Property 21: 30日より古い注文は返品不可**
  - **検証: 要件 17.4**

- [ ] 40. 出荷管理APIエンドポイントの実装
  - GET /api/shipments/picking-list
  - POST /api/shipments/:id/confirm-picking
  - POST /api/shipments
  - POST /api/returns
  - _要件: 15.1, 15.3, 16.1, 17.1_

## 得意先・仕入先管理コンテキスト

- [ ] 41. 得意先集約の実装
  - Customerエンティティ
  - 連絡先情報管理
  - クレジットカード情報の暗号化
  - _要件: 18.1, 18.2, 18.3, 18.4, 18.5_

- [ ] 42. 仕入先集約の実装
  - Supplierエンティティ
  - 取引状態管理（activate/deactivate）
  - _要件: 19.1, 19.2, 19.3, 19.4, 19.5_

- [ ] 43. 得意先管理サービスの実装
  - 得意先登録（registerCustomer）
  - 得意先更新（updateCustomer）
  - 配送情報コピー
  - 注文履歴表示
  - _要件: 18.1, 18.2, 18.3, 18.4, 18.5_

- [ ] 44. 仕入先管理サービスの実装
  - 仕入先登録（registerSupplier）
  - 仕入先更新（updateSupplier）
  - 仕入先無効化/再有効化
  - 参照整合性の維持
  - _要件: 19.1, 19.2, 19.3, 19.4, 19.5_

- [ ] 45. 得意先・仕入先管理APIエンドポイントの実装
  - POST /api/customers
  - PUT /api/customers/:id
  - GET /api/customers/:id
  - POST /api/suppliers
  - PUT /api/suppliers/:id
  - PATCH /api/suppliers/:id/deactivate
  - PATCH /api/suppliers/:id/reactivate
  - _要件: 18.1, 18.2, 18.5, 19.1, 19.2, 19.3, 19.4_

## データ永続化とシリアライゼーション

- [ ] 46. データベーストランザクション管理の実装
  - トランザクション開始/コミット/ロールバック
  - 参照整合性の維持
  - データ制約のバリデーション
  - _要件: 20.3, 20.4, 20.5_

- [ ]* 46.1 トランザクションのプロパティベーステスト
  - **Property 22: トランザクション失敗時のロールバック**
  - **検証: 要件 20.4**

- [ ] 47. シリアライゼーション/デシリアライゼーションの実装
  - エンティティのシリアライズ
  - エンティティのデシリアライズ
  - 型安全性の確保
  - _要件: 20.1, 20.2_

- [ ]* 47.1 シリアライゼーションのプロパティベーステスト
  - **Property 23: シリアライゼーションのラウンドトリップ**
  - **検証: 要件 20.1, 20.2**

## フロントエンド実装

- [ ] 48. 認証UIの実装
  - ログインページ
  - ログアウト機能
  - 認証状態管理（Context API）
  - 保護されたルート
  - _要件: 1.1, 1.5_

- [ ] 49. 商品閲覧UIの実装（得意先向け）
  - 商品一覧ページ
  - 商品詳細ページ
  - カート管理UI
  - _要件: 5.1, 5.2, 5.3, 5.4, 5.5_

- [ ] 50. 注文UIの実装（得意先向け）
  - 注文フォーム
  - 配送情報入力
  - 注文確認画面
  - 注文履歴表示
  - 注文変更/キャンセル
  - _要件: 6.1, 6.2, 6.3, 6.4, 8.1, 9.1_

- [ ] 51. 商品管理UIの実装（スタッフ向け）
  - 商品登録フォーム
  - 商品一覧・編集
  - 販売制御UI
  - 単品関連付けUI
  - _要件: 3.1, 3.2, 3.3, 4.1, 4.2, 4.3_

- [ ] 52. 受注管理UIの実装（スタッフ向け）
  - 受注一覧
  - 受注詳細表示
  - 在庫引当UI
  - 注文確認処理
  - _要件: 7.1, 7.2, 7.3, 7.4_

- [ ] 53. 在庫管理UIの実装（スタッフ向け）
  - 単品登録フォーム
  - 在庫一覧表示
  - 在庫推移グラフ
  - 仕入先管理UI
  - _要件: 10.1, 10.2, 11.5, 12.1, 12.5_

- [ ] 54. 発注管理UIの実装（スタッフ向け）
  - 発注作成フォーム
  - 発注一覧
  - 入荷・検収UI
  - _要件: 13.1, 13.2, 14.1, 14.2_

- [ ] 55. 出荷管理UIの実装（スタッフ向け）
  - ピッキングリスト表示
  - 出荷実行UI
  - 返品処理UI
  - _要件: 15.1, 15.3, 16.1, 17.1_

## テストとCI/CD

- [ ] 56. Checkpoint - すべてのテストが通ることを確認
  - すべてのテストを実行し、パスすることを確認
  - カバレッジが80%以上であることを確認
  - 質問があればユーザーに確認

- [ ] 57. E2Eテストの実装
  - ユーザー登録・ログインフロー
  - 商品注文フロー
  - 在庫管理フロー
  - _要件: 全体_

- [ ] 58. CI/CDパイプラインの設定
  - GitHub Actionsワークフローの作成
  - 自動テスト実行
  - カバレッジレポート
  - 品質ゲートの設定
  - _要件: 全体_

- [ ] 59. ドキュメントの作成
  - README.md（セットアップ手順）
  - API仕様書（OpenAPI）
  - 開発者ガイド
  - デプロイメントガイド
  - _要件: 全体_

- [ ] 60. Final Checkpoint - 本番環境への準備
  - すべてのテストがパスすることを確認
  - セキュリティチェック
  - パフォーマンステスト
  - 質問があればユーザーに確認
