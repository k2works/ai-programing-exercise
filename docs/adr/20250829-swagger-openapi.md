# OpenAPI/Swagger 統合によるAPI開発者体験向上

日付: 2025-08-29

## ステータス

2025-08-29 採択済み

## コンテキスト

- 会議室予約システム（MRS）でREST APIを実装中、開発者体験（DX）の向上が求められている
- API仕様の文書化、テスト、動作確認を効率化したい
- 既存のJWT認証（Bearer Token）とSpring Bootベースの開発環境で、API仕様の自己文書化と動作確認を実現したい
- フロントエンド開発者やテスターが仕様を理解し、APIを直接テストできる環境が必要
- OpenAPI 3.0標準に準拠した仕様管理を導入したい

## 決定

1) **OpenAPI統合ライブラリ**
- 採用: `springdoc-openapi-starter-webmvc-ui:2.6.0`
- 理由: Spring Boot 3.x対応、Spring Security統合、アノテーションベース設定

2) **Swagger UI設定**
- アクセス: `gradle bootRun` → http://localhost:8080/swagger-ui.html
- 有効化: dev環境のみ（`application-dev.properties`）
- JWT認証: 「Authorize」ボタンでBearerトークン設定機能

3) **API仕様管理**
- 形式: OpenAPI 3.0仕様
- アノテーション: `@Operation`, `@Parameter`, `@Schema`, `@SecurityRequirement`
- 自動生成: コントローラーからAPI仕様を自動抽出
- タグ分類: Authentication, Rooms, Reservations

4) **開発フロー**
- APIテスト手順:
  1. `gradle bootRun`でアプリ起動
  2. Swagger UIアクセス
  3. `/api/auth/login`でJWT取得
  4. Authorizeボタンでトークン設定
  5. 保護APIの動作確認

## 影響

**メリット:**
- **開発効率**: API仕様の手動メンテナンス不要、リアルタイム更新
- **テスト効率**: ブラウザからAPI直接テスト、Postmanの代替
- **コミュニケーション**: フロントエンド開発者との仕様共有が簡素化
- **品質向上**: リクエスト/レスポンス形式の可視化、バリデーション確認
- **標準準拠**: OpenAPI 3.0によるAPI仕様の標準化

**デメリット/リスク:**
- **本番除外**: 本番環境でのSwagger UI露出リスク（設定で回避）
- **メンテナンス**: アノテーション保守、スキーマの複雑化
- **パフォーマンス**: 開発時の若干のオーバーヘッド
- **セキュリティ**: JWT管理の適切な実装が前提

**代替案:**
- **手動API文書**: Markdown/Confluenceでの文書管理（更新負荷大）
- **Postman Collection**: APIテスト専用ツール（仕様文書化不足）
- **独自API Docs**: カスタム実装（開発コスト大）

## コンプライアンス

この決定が遵守されていることの確認方法:

1. **依存関係**: `build.gradle`に`springdoc-openapi-starter-webmvc-ui`が追加されている
2. **設定**: `application-dev.properties`にSpringDoc設定が存在する
3. **アクセス**: http://localhost:8080/swagger-ui.html でSwagger UIにアクセス可能
4. **認証統合**: Swagger UIで「Authorize」ボタンが表示され、JWT設定可能
5. **API仕様**: コントローラーにOpenAPIアノテーションが適切に付与されている
6. **タグ分類**: API群が論理的にグループ化されている
7. **本番除外**: 本番環境でSwagger UIが無効化されている

## 備考

**参照:**
- docs/design/アーキテクチャ.md (Section 5: API設計・開発者体験)
- docs/adr/20250829.md (JWT認証決定記録)
- [springdoc-openapi documentation](https://springdoc.org/)
- [OpenAPI 3.0 Specification](https://spec.openapis.org/oas/v3.0.3/)

**実装ガイド:**
- アノテーション例: `@Operation(summary = "ユーザー認証")`
- セキュリティ: `@SecurityRequirement(name = "bearerAuth")`
- スキーマ: `@Schema(example = "user1")`
- 環境分離: `@Profile("dev")`での設定クラス分離

**次のステップ:**
1. build.gradleに依存関係追加
2. OpenAPI設定クラス作成
3. 既存コントローラーにアノテーション追加
4. application-dev.propertiesにSwagger UI設定
5. 動作確認とドキュメント更新
