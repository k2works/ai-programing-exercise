---
title: テスト駆動開発から始めるC#入門 ~ソフトウェア開発の三種の神器を準備する~
description: C#/.NETを使ってソフトウェア開発の三種の神器（バージョン管理、テスティング、自動化）を準備し、継続的な品質改善の環境を整備する方法を学びます。
published: true
date: 2025-01-15T10:00:00.000Z
tags: C#, .NET, TDD, テスト駆動開発, 自動化, 品質管理
editor: markdown
dateCreated: 2025-01-15T10:00:00.000Z
---

# エピソード2

## 初めに

この記事は テスト駆動開発から始めるC#入門 ~2時間でTDDとリファクタリングのエッセンスを体験する~ の続編です。

## 自動化から始めるテスト駆動開発

エピソード1ではテスト駆動開発のゴールが **動作するきれいなコード** であることを学びました。では、良いコードを書き続けるためには何が必要になるでしょうか？それは[ソフトウェア開発の三種の神器](https://t-wada.hatenablog.jp/entry/clean-code-that-works)と呼ばれるものです。

> 今日のソフトウェア開発の世界において絶対になければならない3つの技術的な柱があります。
> 三本柱と言ったり、三種の神器と言ったりしていますが、それらは
> 
>   - バージョン管理
> 
>   - テスティング
> 
>   - 自動化
> 
> の3つです。
> 
> —  https://t-wada.hatenablog.jp/entry/clean-code-that-works 

**バージョン管理** と **テスティング** に関してはエピソード1で触れました。本エピソードでは最後の **自動化** に関しての解説と次のエピソードに備えたセットアップ作業を実施しておきたいと思います。ですがその前に **バージョン管理** で1つだけ解説しておきたいことがありますのでそちらから進めて行きたいと思います。

### コミットメッセージ

これまで作業の区切りにごとにレポジトリにコミットしていましたがその際に以下のような書式でメッセージを書いていました。

```bash
$ git commit -m 'refactor: メソッドの抽出'
```

この書式は [Angularルール](https://github.com/angular/angular.js/blob/master/DEVELOPERS.md#type)に従っています。具体的には、それぞれのコミットメッセージはヘッダ、ボディ、フッタで構成されています。ヘッダはタイプ、スコープ、タイトルというフォーマットで構成されています。

    <タイプ>(<スコープ>): <タイトル>
    <空行>
    <ボディ>
    <空行>
    <フッタ>

ヘッダは必須です。 ヘッダのスコープは任意です。 コミットメッセージの長さは50文字までにしてください。

（そうすることでその他のGitツールと同様にGitHub上で読みやすくなります。）

コミットのタイプは次を用いて下さい。

  - feat: A new feature (新しい機能)

  - fix: A bug fix (バグ修正)

  - docs: Documentation only changes (ドキュメント変更のみ)

  - style: Changes that do not affect the meaning of the code
    (white-space, formatting, missing semi-colons, etc) (コードに影響を与えない変更)

  - refactor: A code change that neither fixes a bug nor adds a feature
    (機能追加でもバグ修正でもないコード変更)

  - perf: A code change that improves performance (パフォーマンスを改善するコード変更)

  - test: Adding missing or correcting existing tests
    (存在しないテストの追加、または既存のテストの修正)

  - chore: Changes to the build process or auxiliary tools and libraries
    such as documentation generation
    (ドキュメント生成のような、補助ツールやライブラリやビルドプロセスの変更)

コミットメッセージにつけるプリフィックスに関しては [【今日からできる】コミットメッセージに 「プレフィックス」をつけるだけで、開発効率が上がった話](https://qiita.com/numanomanu/items/45dd285b286a1f7280ed)を参照ください。

### パッケージマネージャ

では **自動化** の準備に入りたいのですがそのためにはいくつかの外部ライブラリを利用する必要があります。.NET では **NuGet** がパッケージマネージャとしての役割を果たします。

> NuGetとは、.NET用のパッケージマネージャーです。開発者が作成・共有したライブラリやツールを「パッケージ」として配布・利用することができます。

**NuGet** はすでに何度か使っています。例えばテスティングフレームワークの `xunit` のインストールなどです。

```bash
$ dotnet add package xunit
```

しかし、C#/.NETプロジェクトではプロジェクトファイル（`.csproj`）にパッケージの依存関係が記述されます。これにより、プロジェクトをクローンした際に `dotnet restore` を実行するだけで必要なパッケージが自動的にインストールされます。

実際にプロジェクトファイルを見てみましょう。

```xml
<Project Sdk="Microsoft.NET.Sdk">

  <PropertyGroup>
    <TargetFramework>net8.0</TargetFramework>
    <ImplicitUsings>enable</ImplicitUsings>
    <Nullable>enable</Nullable>
  </PropertyGroup>

  <ItemGroup>
    <PackageReference Include="System.Collections.Immutable" Version="9.0.0" />
    <PackageReference Include="Microsoft.CodeAnalysis.Analyzers" Version="3.11.0">
      <PrivateAssets>all</PrivateAssets>
      <IncludeAssets>runtime; build; native; contentfiles; analyzers; buildtransitive</IncludeAssets>
    </PackageReference>
  </ItemGroup>

</Project>
```

これで、次の準備ができました。パッケージの依存関係がプロジェクトファイルに明記されているため、どの環境でも `dotnet restore` で同じ依存関係を復元できます。

### 静的コード解析

良いコードを書き続けるためにはコードの品質を維持していく必要があります。エピソード1では **テスト駆動開発** によりプログラムを動かしながら品質の改善していきました。出来上がったコードに対する品質チェックの方法として **静的コード解析** があります。C#/.NET用の **静的コード解析** ツールとして[Microsoft.CodeAnalysis.Analyzers](https://github.com/dotnet/roslyn-analyzers) を使って確認してみましょう。

まず、アナライザパッケージをインストールしましょう：

```bash
$ dotnet add package Microsoft.CodeAnalysis.Analyzers
```

次に、コード分析の設定ファイルを作成します。`.editorconfig` ファイルでコーディングスタイルを定義できます：

```ini
root = true

[*]
indent_style = space
indent_size = 4
end_of_line = crlf
charset = utf-8
trim_trailing_whitespace = true
insert_final_newline = true

[*.cs]
# C# coding conventions
dotnet_sort_using_directives = true
dotnet_separate_import_directive_groups = false

# Language rules
csharp_prefer_var = false:suggestion
csharp_prefer_braces = true:warning
```

`.globalconfig` ファイルでアナライザルールを設定できます：

```ini
is_global = true

# Enable all analyzer rules
dotnet_analyzer_diagnostic.category-design.severity = warning
dotnet_analyzer_diagnostic.category-documentation.severity = suggestion
dotnet_analyzer_diagnostic.category-globalization.severity = warning
dotnet_analyzer_diagnostic.category-maintainability.severity = warning
dotnet_analyzer_diagnostic.category-naming.severity = warning
dotnet_analyzer_diagnostic.category-performance.severity = warning
dotnet_analyzer_diagnostic.category-reliability.severity = warning
dotnet_analyzer_diagnostic.category-security.severity = warning
dotnet_analyzer_diagnostic.category-style.severity = suggestion
dotnet_analyzer_diagnostic.category-usage.severity = warning
```

コード分析を実行してみましょう：

```bash
$ dotnet build
```

ビルド時に静的コード解析が実行され、コードの品質チェックが行われます。

```bash
$ git add .
$ git commit -m 'chore: 静的コード解析の設定'
```

### コードフォーマッタ

**コードフォーマッタ** は開発チーム内でのコーディングスタイルを統一するためのツールです。C#/.NETでは `dotnet format` コマンドが標準で提供されています。

コードのフォーマットを実行してみましょう：

```bash
$ dotnet format
```

このコマンドにより、プロジェクト内のすべてのC#ファイルが `.editorconfig` で定義されたルールに従ってフォーマットされます。

フォーマットの確認のみを行いたい場合は：

```bash
$ dotnet format --verify-no-changes
```

実際に何か変更があるかどうかを確認してみましょう：

```bash
$ dotnet format --verify-no-changes
```

このコマンドが成功すれば、コードは既に正しくフォーマットされています。

```bash
$ git add .
$ git commit -m 'chore: コードフォーマッタの導入'
```

### コードカバレッジ

**コードカバレッジ** はテストがプロダクションコードをどのくらいカバーしているかを測る指標です。C#/.NETでは `coverlet` がよく使われます。

コードカバレッジツールをインストールしましょう：

```bash
$ dotnet add package coverlet.collector
$ dotnet add package coverlet.msbuild
```

カバレッジレポートを生成するために `reportgenerator` ツールもインストールしましょう：

```bash
$ dotnet tool install -g dotnet-reportgenerator-globaltool
```

テストをカバレッジ付きで実行してみましょう：

```bash
$ dotnet test --collect:"XPlat Code Coverage"
```

HTML形式のレポートを生成します：

```bash
$ reportgenerator -reports:"**/coverage.cobertura.xml" -targetdir:"coverage" -reporttypes:Html
```

生成された `coverage/index.html` をブラウザで開くとカバレッジレポートを確認できます。

ワンコマンドでカバレッジレポートを生成するスクリプトを作成しましょう：

```bash
#!/bin/bash
# coverage.sh
dotnet test --collect:"XPlat Code Coverage" --results-directory ./TestResults
reportgenerator -reports:"./TestResults/**/coverage.cobertura.xml" -targetdir:"coverage" -reporttypes:Html
echo "Coverage report generated in coverage/index.html"
```

```bash
$ chmod +x coverage.sh
$ ./coverage.sh
```

```bash
$ git add .
$ git commit -m 'chore: コードカバレッジの導入'
```

### タスクランナー

良いコードを書き続けるために今まで色々なツールを導入してきましたが毎回個別に実行するのは面倒です。そこで **タスクランナー** の出番です。C#/.NETプロジェクトでは、Visual Studio Codeの **tasks.json** を使ってタスクを定義できます。

`.vscode/tasks.json` ファイルを作成して、よく使うタスクを定義しましょう：

```json
{
    "version": "2.0.0",
    "tasks": [
        {
            "label": "build",
            "command": "dotnet",
            "type": "process",
            "args": ["build"],
            "group": "build",
            "problemMatcher": "$msCompile"
        },
        {
            "label": "test",
            "command": "dotnet",
            "type": "process",
            "args": ["test"],
            "group": "test",
            "problemMatcher": "$msCompile"
        },
        {
            "label": "format",
            "command": "dotnet",
            "type": "process",
            "args": ["format"],
            "group": "build"
        },
        {
            "label": "coverage",
            "command": "bash",
            "type": "shell",
            "args": ["-c", "dotnet test --collect:'XPlat Code Coverage' --results-directory ./TestResults && reportgenerator -reports:'./TestResults/**/coverage.cobertura.xml' -targetdir:'coverage' -reporttypes:Html"],
            "group": "test"
        },
        {
            "label": "all-quality-checks",
            "dependsOrder": "sequence",
            "dependsOn": ["format", "build", "test", "coverage"],
            "group": "build"
        }
    ]
}
```

これらのタスクはVS Codeのコマンドパレット（Ctrl+Shift+P）から「Tasks: Run Task」を選択して実行できます。

コマンドラインからも実行可能です（VS Code拡張が必要）：

```bash
$ code --task all-quality-checks
```

または、dotnet CLIのみを使用する場合：

```bash
$ dotnet format && dotnet build && dotnet test && ./coverage.sh
```

```bash
$ git add .
$ git commit -m 'chore: タスクランナーの導入'
```

### タスクの自動化

**タスクランナー** により品質をチェックするコマンドは用意されましたが、まだ手動で実行する必要があります。ファイルの変更を検出して自動的にタスクを実行する仕組みがあると便利です。Rubyの **Guard** のような役割を果たすツールを作成しましょう。

Linuxの `inotify-tools` を使ってファイル監視スクリプトを作成します：

```bash
$ sudo apt-get update && sudo apt-get install -y inotify-tools
```

ファイル監視と自動実行のスクリプト `watch.sh` を作成します：

```bash
#!/bin/bash
# watch.sh - C#/.NET プロジェクトのファイル監視と自動品質チェック

echo "=== C#/.NET ファイル監視開始 ==="
echo "ファイル変更を監視中..."
echo "終了するには Ctrl+C を押してください"
echo ""

# 初回実行
echo "🔄 初回品質チェック実行..."
dotnet format --verify-no-changes > /dev/null 2>&1 || dotnet format
dotnet build
if [ $? -eq 0 ]; then
    dotnet test --no-build
    if [ $? -eq 0 ]; then
        echo "✅ 初回チェック完了"
    else
        echo "❌ テスト失敗"
    fi
else
    echo "❌ ビルド失敗"
fi
echo ""

# ファイル監視開始
inotifywait -m -r -e modify,create,delete --include='\.cs$' . | while read path action file; do
    echo "📝 変更検出: $file ($action)"
    
    # フォーマット
    echo "🔄 コードフォーマット実行..."
    dotnet format --verify-no-changes > /dev/null 2>&1 || dotnet format
    
    # ビルド
    echo "🔨 ビルド実行..."
    dotnet build --no-restore > /dev/null 2>&1
    if [ $? -eq 0 ]; then
        echo "✅ ビルド成功"
        
        # テスト
        echo "🧪 テスト実行..."
        dotnet test --no-build > /dev/null 2>&1
        if [ $? -eq 0 ]; then
            echo "✅ テスト成功"
        else
            echo "❌ テスト失敗"
        fi
    else
        echo "❌ ビルド失敗"
    fi
    echo ""
done
```

スクリプトを実行可能にして実行します：

```bash
$ chmod +x watch.sh
$ ./watch.sh
```

これで、C#ファイルが変更されるたびに自動的にフォーマット、ビルド、テストが実行されます。

別ターミナルで何かファイルを変更してみてください。変更が検出されて自動的にタスクが実行されることが確認できるはずです。

テストがパスすることが確認できたらコミットしておきましょう。このときターミナルでは `watch.sh` が動いているので別ターミナルを開いてコミットを実施すると良いでしょう。

```bash
$ git add .
$ git commit -m 'chore: タスクの自動化'
```

これで [ソフトウェア開発の三種の神器](https://t-wada.hatenablog.jp/entry/clean-code-that-works) の最後のアイテムの準備ができました。次回の開発からは最初にコマンドラインで `./watch.sh` を実行すれば良いコードを書くためのタスクを自動でやってくれるようになるのでコードを書くことに集中できるようになりました。では、次のエピソードに進むとしましょう。

## まとめ

このエピソードでは、C#/.NETプロジェクトにおけるソフトウェア開発の三種の神器を整備しました：

1. **バージョン管理**: Angularルールに従ったコミットメッセージの標準化
2. **テスティング**: xUnitを使ったテストの実行とカバレッジ測定
3. **自動化**: ファイル監視による品質チェックの自動実行

これらのツールにより、以下が実現できるようになりました：

- **コード品質の維持**: 静的コード解析とフォーマッタによる一貫したコーディングスタイル
- **継続的なテスト**: ファイル変更時の自動テスト実行
- **可視化**: コードカバレッジレポートによる品質の可視化
- **開発効率の向上**: 手動作業の自動化による開発サイクルの高速化

次のエピソードからは、これらの環境を活用してより複雑な機能の開発にテスト駆動開発で取り組んでいきます。
