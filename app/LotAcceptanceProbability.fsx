// ロット合格確率の計算

// 基本データ
let total = 100      // 全部の個数
let defective = 10   // 不良品の個数
let good = total - defective  // 良品の個数 = 90
let samples = 3      // 抜き取る個数

// 方法1: 順番にかけ算していく
let probability1 =
    let p1 = float good / float total           // 90/100
    let p2 = float (good - 1) / float (total - 1)  // 89/99
    let p3 = float (good - 2) / float (total - 2)  // 88/98
    p1 * p2 * p3

printfn "方法1（直接計算）: %.6f" probability1

// 方法2: 再帰関数でかけ算（何個取っても対応できる）
let rec calculateProbability goodRemaining totalRemaining samplesLeft =
    if samplesLeft = 0 then
        1.0  // 全部引き終わった → 成功！
    else
        let p = float goodRemaining / float totalRemaining
        p * calculateProbability (goodRemaining - 1) (totalRemaining - 1) (samplesLeft - 1)

let probability2 = calculateProbability good total samples
printfn "方法2（再帰関数）: %.6f" probability2

// 方法3: fold を使う（関数型っぽい書き方）
let probability3 =
    [0 .. samples - 1]
    |> List.fold (fun acc i ->
        acc * float (good - i) / float (total - i)
    ) 1.0

printfn "方法3（fold使用）: %.6f" probability3

// パーセント表示
printfn "\n合格確率: %.2f%%" (probability3 * 100.0)
