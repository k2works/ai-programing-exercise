# IT-1 起動確認ジャーナル — US-001

**日付**: 2026-05-02
**対象ストーリー**: US-001 (Mod がクライアントで起動することを確認)

## 確認した設定

| 項目 | 値 | ファイル |
|------|----|----|
| MODID | `aipe` | `apps/aipe/src/main/java/com/k2works/aipe/AiProgrammingExercise.java` (`public static final String MODID = "aipe";`) |
| mod_id（Gradle プロパティ） | `aipe` | `apps/aipe/gradle.properties` |
| mod_name | `AiProgrammingExercise` | `apps/aipe/gradle.properties` |
| Mod 主クラス | `com.k2works.aipe.AiProgrammingExercise` | `@Mod(AiProgrammingExercise.MODID)` |
| Minecraft バージョン | 1.21.11 | `apps/aipe/gradle.properties` |

`META-INF/neoforge.mods.toml` は `apps/aipe/src/main/templates/META-INF/neoforge.mods.toml` をテンプレートにし、Gradle の `generateModMetadata` タスクで `mod_id`、`mod_name` 等を展開して生成している。

## 起動確認手順

### コマンド

```powershell
cd apps/aipe
./gradlew runClient
```

### 期待動作

1. Gradle が NeoForge / Minecraft 1.21.11 のアセットをダウンロード（初回 5〜15 分）
2. Minecraft クライアントが起動
3. コンソールログに `aipe` Mod のロードメッセージが出力される
4. タイトル画面が表示される

### 結果

ローカル環境でユーザーが実行し、Mod ID `aipe` のロードを目視確認（2026-05-02）。クライアントは正常起動した。

## 注意事項

- `runClient` は GUI を伴うため CI（ヘッドレス）では実行不可。CI では `build` と `test`（JUnit 5）のみ。
- Mod ロードの完全自動保証は IT-2 の SmokeGameTest（`runGameTestServer`）緑化を待つ。

## 関連

- [イテレーション 1 計画](../development/iteration_plan-1.md)
- [ユーザーストーリー US-001](../requirements/user_stories.md)
