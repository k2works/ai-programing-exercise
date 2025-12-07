// NANDゲート：両方trueのときだけfalse
let nand a b = not (a && b)

// 回路の構造
let circuit x y =
    let top = nand x x      // 上のNAND（Xの反対）
    let bottom = nand y y   // 下のNAND（Yの反対）
    nand top bottom         // 右のNAND

// 真理値表を表示
printfn "=== NAND ゲートで OR 回路を実装 ==="
printfn ""
for x in [false; true] do
    for y in [false; true] do
        let z = circuit x y
        let orResult = x || y   // 比較用：OR
        printfn "X=%b Y=%b → Z=%b （X OR Y = %b）" x y z orResult
