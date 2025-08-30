# ArchUnitによるアーキテクチャテスト導入

日付: 2025-08-30

## ステータス

2025-08-30 採択済み

## コンテキスト

- 本プロジェクトではヘキサゴナルアーキテクチャ（Ports & Adapters）を採用している
- アーキテクチャの境界とレイヤー分離が適切に維持されることを保証する必要がある
- 手動でのアーキテクチャレビューでは、以下の問題を完全に防ぐことは困難：
  - ドメイン層がインフラ層に依存する循環依存
  - アプリケーション層を経由せずにプレゼンテーション層からドメイン層への直接アクセス
  - パッケージ命名規則の違反
  - 依存方向の逆転（DIP違反）
- 継続的にアーキテクチャ品質を維持し、技術的負債の蓄積を防ぐ仕組みが必要
- 既存の品質ゲート（SpotBugs、CheckStyle、PMD、テストカバレッジ）に加えて、アーキテクチャレベルでの検証が求められる

## 決定

### 1) ArchUnit導入
- 採用: ArchUnit (`com.tngtech.archunit:archunit-junit5`)
- 配置: `src/test/java/mrs/architecture/` パッケージ
- 実行: Gradleビルドプロセスに統合（`test` タスクの一部）

### 2) 検証ルール
実装する主要なアーキテクチャルール：

#### レイヤー依存関係
```java
// ドメイン層は他の層に依存しない（純粋性の保証）
classes().that().resideInAPackage("..domain..")
    .should().onlyDependOnClassesThat()
    .resideInAnyPackage("..domain..", "java..", "javax..", "edu.umd.cs.findbugs..");

// インフラ層はドメイン層を知っているが、逆は許可しない
classes().that().resideInAPackage("..infrastructure..")
    .should().dependOnClassesThat()
    .resideInAnyPackage("..application..", "..domain..", "java..", "org.springframework..");
```

#### ヘキサゴナルアーキテクチャ
```java
// ポートとアダプターの命名規約
classes().that().resideInAPackage("..port.in..")
    .should().beInterfaces()
    .andShould().haveSimpleNameEndingWith("UseCase");

classes().that().resideInAPackage("..port.out..")
    .should().beInterfaces()
    .andShould().haveSimpleNameEndingWith("Port");

// アダプターの実装
classes().that().resideInAPackage("..infrastructure..")
    .and().haveSimpleNameEndingWith("Adapter")
    .should().implement(JavaClass.Predicates.resideInAPackage("..port.out.."));
```

#### パッケージ構造
```java
// 各レイヤーのパッケージ存在検証
classes().that().resideInAPackage("..application.domain..")
    .should().onlyBeAccessed().byClassesThat()
    .resideInAnyPackage("..application..", "..infrastructure..", "..test..");
```

### 3) テスト構成
- `ArchitectureTest.java`: メインのアーキテクチャルールテスト
- `LayerDependencyTest.java`: レイヤー間依存関係の検証
- `HexagonalArchitectureTest.java`: ポート＆アダプターパターンの検証
- `NamingConventionTest.java`: 命名規約の検証

## 影響

### メリット
- **自動化されたアーキテクチャ検証**: 手動レビューに依存せず、CIパイプラインで継続的にアーキテクチャ品質を保証
- **技術的負債の早期発見**: 不適切な依存関係や設計違反を開発時点で検出
- **開発者の学習促進**: アーキテクチャルールを明示的に定義することで、チーム全体の理解を深める
- **リファクタリングの安全性向上**: 大規模な変更時でもアーキテクチャ原則が維持されることを保証
- **ドキュメンテーション効果**: コードとして記述されたアーキテクチャルールは生きたドキュメントとなる

### デメリット/リスク
- **テストメンテナンス負荷**: アーキテクチャ変更時にはテストルールの更新が必要
- **学習コスト**: ArchUnit固有のAPIと概念の理解が必要
- **誤検知リスク**: 過度に厳密なルールが正当な実装パターンを阻害する可能性
- **パフォーマンス影響**: 大規模コードベースでのArchUnitテスト実行時間増加

### 代替案
- **静的解析ツール拡張**: SonarQubeのアーキテクチャルールプラグイン使用
  - 利点: 統合環境での一元管理
  - 欠点: カスタムルール記述の柔軟性が低い
- **手動アーキテクチャレビュー**: プルリクエスト時の人的チェック
  - 利点: 導入コストなし
  - 欠点: 人的ミス、レビュー品質のばらつき

## コンプライアンス

この決定が遵守されていることの確認方法：

### 1) 依存関係
- `build.gradle`に`com.tngtech.archunit:archunit-junit5`依存関係が追加されている
- テスト実行時にArchUnitのバージョン情報がログに出力される

### 2) テストファイル
- `src/test/java/mrs/architecture/`パッケージに以下のテストファイルが存在：
  - `ArchitectureTest.java`
  - `LayerDependencyTest.java` 
  - `HexagonalArchitectureTest.java`
  - `NamingConventionTest.java`

### 3) CI統合
- `./gradlew test`実行時にArchUnitテストが自動実行される
- アーキテクチャ違反が検出された場合はビルドが失敗する
- テストレポートにArchUnitの結果が含まれている

### 4) カバレッジ
- 主要なアーキテクチャ制約（レイヤー依存、命名規約、パッケージ構造）がテストでカバーされている
- 新規パッケージ追加時は対応するArchUnitルールも追加される

## 備考

### 実装メモ
- **段階的導入**: 既存コードに対しては警告レベルから開始し、新規コードは厳密適用
- **ルールの粒度**: 過度に細かいルールは避け、重要なアーキテクチャ原則に焦点
- **例外処理**: 正当な例外ケース（テストコード、設定クラス等）は適切に除外

### 関連ドキュメント
- `docs/design/アーキテクチャ.md`: ヘキサゴナルアーキテクチャの設計方針
- `docs/adr/20250829.md`: 技術スタック決定（MyBatis/JWT/Cucumber）
- `docs/reference/よいソフトウェアとは.md`: 品質方針とアーキテクチャ原則

### 参考資料
- [ArchUnit User Guide](https://www.archunit.org/userguide/html/000_Index.html)
- [Hexagonal Architecture with ArchUnit](https://github.com/TNG/ArchUnit-Examples)