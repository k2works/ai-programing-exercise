///////////////////////////////////////////////////////////////////////////////
// ARGUMENTS
///////////////////////////////////////////////////////////////////////////////

var target = Argument("target", "Default");
var configuration = Argument("configuration", "Release");

///////////////////////////////////////////////////////////////////////////////
// SETUP / TEARDOWN
///////////////////////////////////////////////////////////////////////////////

Setup(ctx =>
{
    Information("=== F# データベース設計プロジェクト ビルド開始 ===");
    Information($"ターゲット: {target}");
    Information($"構成: {configuration}");
});

Teardown(ctx =>
{
    Information("=== ビルド終了 ===");
});

///////////////////////////////////////////////////////////////////////////////
// TASKS
///////////////////////////////////////////////////////////////////////////////

Task("Clean")
    .Description("ビルド成果物のクリーンアップ")
    .Does(() =>
{
    CleanDirectories("./*/bin");
    CleanDirectories("./*/obj");
    CleanDirectories("./TestResults");
    CleanDirectory("./coverage");
    Information("✅ クリーンアップ完了");
});

Task("Restore")
    .Description("NuGetパッケージの復元")
    .Does(() =>
{
    DotNetRestore("./SalesManagement.sln");
    Information("✅ パッケージ復元完了");
});

Task("Lint")
    .Description("F#コード静的解析")
    .IsDependentOn("Restore")
    .Does(() =>
{
    var exitCode = StartProcess("dotnet", new ProcessSettings {
        Arguments = "fsharplint lint SalesManagement.sln"
    });

    if (exitCode != 0)
    {
        Warning("⚠️ FSharpLintで警告が検出されました");
    }
    else
    {
        Information("✅ FSharpLint チェックOK");
    }
});

Task("Build")
    .Description("プロジェクトのビルド")
    .IsDependentOn("Restore")
    .Does(() =>
{
    DotNetBuild("./SalesManagement.sln", new DotNetBuildSettings
    {
        Configuration = configuration,
        NoRestore = true
    });
    Information("✅ ビルド完了");
});

Task("Test")
    .Description("テスト実行")
    .IsDependentOn("Build")
    .Does(() =>
{
    DotNetTest("./SalesManagement.sln", new DotNetTestSettings
    {
        Configuration = configuration,
        NoBuild = true,
        Verbosity = DotNetVerbosity.Normal
    });
    Information("✅ テスト完了");
});

Task("Test-Coverage")
    .Description("カバレッジ測定付きテスト実行")
    .IsDependentOn("Build")
    .Does(() =>
{
    DotNetTest("./SalesManagement.sln", new DotNetTestSettings
    {
        Configuration = configuration,
        NoBuild = true,
        ArgumentCustomization = args => args
            .Append("--collect:\"XPlat Code Coverage\"")
            .Append("--results-directory ./TestResults")
    });
    Information("✅ テスト完了（カバレッジ測定済み）");
});

Task("Coverage-Report")
    .Description("カバレッジレポート生成")
    .IsDependentOn("Test-Coverage")
    .Does(() =>
{
    var reportGeneratorPath = Context.Tools.Resolve("reportgenerator.exe")
        ?? Context.Tools.Resolve("reportgenerator");

    if (reportGeneratorPath == null)
    {
        Warning("reportgeneratorがインストールされていません。インストール中...");
        StartProcess("dotnet", "tool install -g dotnet-reportgenerator-globaltool");
        reportGeneratorPath = Context.Tools.Resolve("reportgenerator");
    }

    var coverageFiles = GetFiles("./TestResults/**/coverage.cobertura.xml");
    if (coverageFiles.Count == 0)
    {
        Warning("カバレッジファイルが見つかりません");
        return;
    }

    var settings = new ProcessSettings
    {
        Arguments = new ProcessArgumentBuilder()
            .AppendQuoted($"-reports:{string.Join(";", coverageFiles)}")
            .AppendQuoted("-targetdir:coverage")
            .Append("-reporttypes:Html")
    };

    StartProcess("reportgenerator", settings);
    Information("✅ カバレッジレポート生成完了: coverage/index.html");
});

Task("CI")
    .Description("CI環境用の完全チェック")
    .IsDependentOn("Clean")
    .IsDependentOn("Lint")
    .IsDependentOn("Build")
    .IsDependentOn("Test-Coverage")
    .IsDependentOn("Coverage-Report")
    .Does(() =>
{
    Information("✅ CIチェック完了");
});

Task("Migrate")
    .Description("データベースマイグレーション実行")
    .Does(() =>
{
    Information("=== マイグレーション実行 ===");

    var settings = new DotNetRunSettings
    {
        WorkingDirectory = "./SalesManagement.Infrastructure",
        Configuration = configuration
    };

    DotNetRun("./SalesManagement.Infrastructure/SalesManagement.Infrastructure.fsproj", settings);
    Information("✅ マイグレーション完了");
});

Task("Docker-Up")
    .Description("Dockerコンテナ起動")
    .Does(() =>
{
    StartProcess("docker-compose", "up -d");
    Information("✅ Dockerコンテナ起動完了");
});

Task("Docker-Down")
    .Description("Dockerコンテナ停止")
    .Does(() =>
{
    StartProcess("docker-compose", "down");
    Information("✅ Dockerコンテナ停止完了");
});

Task("Default")
    .Description("デフォルトタスク（ビルドとテスト）")
    .IsDependentOn("Build")
    .IsDependentOn("Test");

///////////////////////////////////////////////////////////////////////////////
// EXECUTION
///////////////////////////////////////////////////////////////////////////////

RunTarget(target);
