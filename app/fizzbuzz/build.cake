// FizzBuzz F# Project Build Script using Cake

///////////////////////////////////////////////////////////////////////////////
// ARGUMENTS
///////////////////////////////////////////////////////////////////////////////

var target = Argument("target", "Default");
var configuration = Argument("configuration", "Debug");

///////////////////////////////////////////////////////////////////////////////
// PREPARATION
///////////////////////////////////////////////////////////////////////////////

var solutionFile = "./FizzBuzz.sln";
var srcProject = "./src/FizzBuzz.fsproj";
var testProject = "./tests/FizzBuzz.Tests.fsproj";
var testResultsDir = Directory("./TestResults");
var coverageReportsDir = testResultsDir + Directory("html");

///////////////////////////////////////////////////////////////////////////////
// TASKS
///////////////////////////////////////////////////////////////////////////////

Task("Clean")
    .Description("ビルド成果物をクリーンアップ")
    .Does(() =>
{
    DotNetClean(solutionFile);
    CleanDirectory(testResultsDir);
});

Task("Restore")
    .Description("NuGet パッケージを復元")
    .Does(() =>
{
    DotNetRestore(solutionFile);
    StartProcess("dotnet", new ProcessSettings {
        Arguments = "tool restore"
    });
});

Task("Build")
    .Description("プロジェクトをビルド")
    .IsDependentOn("Restore")
    .Does(() =>
{
    DotNetBuild(solutionFile, new DotNetBuildSettings
    {
        Configuration = configuration,
        NoRestore = true
    });
});

Task("Format")
    .Description("コードをフォーマット")
    .Does(() =>
{
    StartProcess("dotnet", new ProcessSettings {
        Arguments = "fantomas src/ tests/"
    });
});

Task("Lint")
    .Description("コードの静的解析（サイクロマティック複雑度チェック含む）")
    .Does(() =>
{
    StartProcess("dotnet", new ProcessSettings {
        Arguments = "dotnet-fsharplint lint --project src/FizzBuzz.fsproj --lint-config fsharplint.json"
    });
});

Task("Test")
    .Description("テストを実行")
    .IsDependentOn("Build")
    .Does(() =>
{
    DotNetTest(testProject, new DotNetTestSettings
    {
        Configuration = configuration,
        NoBuild = true,
        Verbosity = DotNetVerbosity.Normal
    });
});

Task("Coverage")
    .Description("テストカバレッジを収集")
    .IsDependentOn("Build")
    .Does(() =>
{
    DotNetTest(testProject, new DotNetTestSettings
    {
        Configuration = configuration,
        NoBuild = true,
        Collectors = new[] { "XPlat Code Coverage" },
        ResultsDirectory = testResultsDir,
        Verbosity = DotNetVerbosity.Quiet
    });
});

Task("Report")
    .Description("HTMLカバレッジレポートを生成")
    .IsDependentOn("Coverage")
    .Does(() =>
{
    var coverageFiles = GetFiles("./TestResults/*/coverage.cobertura.xml");
    if (coverageFiles.Any())
    {
        StartProcess("dotnet", new ProcessSettings {
            Arguments = $"reportgenerator -reports:{string.Join(";", coverageFiles)} -targetdir:{coverageReportsDir} -reporttypes:Html"
        });
        Information($"カバレッジレポートが生成されました: {coverageReportsDir}/index.html");
    }
    else
    {
        Warning("カバレッジファイルが見つかりませんでした");
    }
});

Task("Run")
    .Description("アプリケーションを実行")
    .IsDependentOn("Build")
    .Does(() =>
{
    DotNetRun(srcProject, new DotNetRunSettings
    {
        Configuration = configuration,
        NoBuild = true
    });
});

Task("Check")
    .Description("コードフォーマット、静的解析、テストを実行")
    .IsDependentOn("Format")
    .IsDependentOn("Lint")
    .IsDependentOn("Test");

Task("All")
    .Description("全てのタスクを実行")
    .IsDependentOn("Clean")
    .IsDependentOn("Build")
    .IsDependentOn("Check");

Task("Default")
    .IsDependentOn("Check");

///////////////////////////////////////////////////////////////////////////////
// EXECUTION
///////////////////////////////////////////////////////////////////////////////

RunTarget(target);