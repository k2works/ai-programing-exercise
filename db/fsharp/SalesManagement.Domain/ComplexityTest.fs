module SalesManagement.Domain.ComplexityTest

/// <summary>
/// サイクロマティック複雑度テスト用の関数
/// 複雑度が15を超えるため、FSharpLintで警告が出るはず
/// サイクロマティック複雑度: 約20
/// </summary>
let highComplexityFunction (value:int) (flag:bool) (status:string) =
    if value = 1 && flag then
        "one-true"
    elif value = 1 && not flag then
        "one-false"
    elif value = 2 && flag then
        "two-true"
    elif value = 2 && not flag then
        "two-false"
    elif value = 3 && flag then
        "three-true"
    elif value = 3 && not flag then
        "three-false"
    elif value = 4 && status = "active" then
        "four-active"
    elif value = 4 && status = "inactive" then
        "four-inactive"
    elif value = 5 && status = "pending" then
        "five-pending"
    elif value = 5 && status = "completed" then
        "five-completed"
    elif value = 6 then
        "six"
    elif value = 7 then
        "seven"
    elif value = 8 then
        "eight"
    elif value = 9 then
        "nine"
    elif value = 10 then
        "ten"
    elif value > 10 && value < 20 then
        "between-10-20"
    elif value >= 20 && flag then
        "twenty-or-more-true"
    elif value >= 20 && not flag then
        "twenty-or-more-false"
    else
        "unknown"

/// <summary>
/// 超高複雑度関数（サイクロマティック複雑度: 約30）
/// 絶対に警告が出るはず
/// </summary>
let superHighComplexityFunction (a:int) (b:int) (c:bool) (d:string) =
    if a = 1 && b = 1 then "1-1"
    elif a = 1 && b = 2 then "1-2"
    elif a = 1 && b = 3 then "1-3"
    elif a = 2 && b = 1 && c then "2-1-true"
    elif a = 2 && b = 1 && not c then "2-1-false"
    elif a = 2 && b = 2 && c then "2-2-true"
    elif a = 2 && b = 2 && not c then "2-2-false"
    elif a = 3 && b = 1 && d = "x" then "3-1-x"
    elif a = 3 && b = 1 && d = "y" then "3-1-y"
    elif a = 3 && b = 2 && d = "x" then "3-2-x"
    elif a = 3 && b = 2 && d = "y" then "3-2-y"
    elif a = 4 && b = 1 then "4-1"
    elif a = 4 && b = 2 then "4-2"
    elif a = 4 && b = 3 then "4-3"
    elif a = 5 && c then "5-true"
    elif a = 5 && not c then "5-false"
    elif a = 6 && d = "a" then "6-a"
    elif a = 6 && d = "b" then "6-b"
    elif a = 7 then "7"
    elif a = 8 then "8"
    elif a = 9 then "9"
    elif a = 10 && b > 5 then "10-gt5"
    elif a = 10 && b <= 5 then "10-le5"
    elif a > 10 && a < 20 && c then "10-20-true"
    elif a > 10 && a < 20 && not c then "10-20-false"
    elif a >= 20 && b = 1 then "ge20-1"
    elif a >= 20 && b = 2 then "ge20-2"
    elif a >= 20 && b >= 3 then "ge20-ge3"
    else "default"

/// <summary>
/// 複雑度が低い関数（比較用）
/// </summary>
let lowComplexityFunction (value:int) =
    match value with
    | 1 -> "one"
    | 2 -> "two"
    | 3 -> "three"
    | _ -> "unknown"
