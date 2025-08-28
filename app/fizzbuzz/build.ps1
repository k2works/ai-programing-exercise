# Cake Build Script Runner for PowerShell
param(
    [string]$Target = "Default"
)

if ($Target -eq "help" -or $Target -eq "--help") {
    Write-Host "使用可能なタスク:"
    & dotnet cake build.cake --showdescription
} else {
    & dotnet cake build.cake --target=$Target
}