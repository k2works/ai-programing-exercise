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
    Information("=== ビルド開始 ===");
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

Task("Format")
    .Description("コードフォーマット")
    .IsDependentOn("Restore")
    .Does(() =>
{
    var exitCode = StartProcess("dotnet", new ProcessSettings {
        Arguments = "format --verify-no-changes"
    });

    if (exitCode != 0)
    {
        Warning("⚠️ フォーマットが必要です。自動修正を実行...");
        StartProcess("dotnet", new ProcessSettings {
            Arguments = "format"
        });
        Information("✅ コードフォーマット完了");
    }
    else
    {
        Information("✅ コードフォーマットOK");
    }
});

Task("Build")
    .Description("プロジェクトのビルド")
    .IsDependentOn("Restore")
    .IsDependentOn("Format")
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

Task("Watch")
    .Description("ファイル変更を監視してテストを自動実行")
    .Does(() =>
{
    var watcher = new System.IO.FileSystemWatcher(".");
    watcher.Path = ".";
    watcher.Filter = "*.cs";
    watcher.IncludeSubdirectories = true;
    watcher.EnableRaisingEvents = true;

    Information("=== ファイル監視開始 ===");
    Information("C#ファイルの変更を監視中...");
    Information("終了するには Ctrl+C を押してください");

    watcher.Changed += (sender, e) => {
        Information($"📝 変更検出: {e.Name}");
        RunTarget("Test");
    };

    // 初回実行
    RunTarget("Test");

    // 監視継続
    while(true)
    {
        System.Threading.Thread.Sleep(1000);
    }
});

Task("CI")
    .Description("CI環境用の完全チェック")
    .IsDependentOn("Clean")
    .IsDependentOn("Format")
    .IsDependentOn("Build")
    .IsDependentOn("Test-Coverage")
    .IsDependentOn("Coverage-Report")
    .Does(() =>
{
    Information("✅ CIチェック完了");
});

Task("Migrate")
    .Description("データベースマイグレーション実行")
    .IsDependentOn("Build")
    .IsDependentOn("Docker-Up")
    .Does(() =>
{
    Information("=== マイグレーション実行 ===");

    var settings = new DotNetRunSettings
    {
        WorkingDirectory = "./SalesManagement.Infrastructure",
        Configuration = configuration
    };

    DotNetRun("SalesManagement.Infrastructure.csproj", settings);
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

Task("Setup")
    .Description("完全セットアップ（Docker起動 + ビルド + マイグレーション + テスト）")
    .IsDependentOn("Docker-Up")
    .IsDependentOn("Build")
    .IsDependentOn("Migrate")
    .IsDependentOn("Test")
    .Does(() =>
{
    Information("✅ セットアップ完了");
});

Task("Reset")
    .Description("環境リセット（Docker停止 + 起動 + マイグレーション）")
    .IsDependentOn("Docker-Down")
    .IsDependentOn("Docker-Up")
    .IsDependentOn("Build")
    .IsDependentOn("Migrate")
    .Does(() =>
{
    Information("✅ 環境リセット完了");
});

Task("Default")
    .Description("デフォルトタスク（ビルドとテスト）")
    .IsDependentOn("Build")
    .IsDependentOn("Test");

///////////////////////////////////////////////////////////////////////////////
// EXECUTION
///////////////////////////////////////////////////////////////////////////////

RunTarget(target);
