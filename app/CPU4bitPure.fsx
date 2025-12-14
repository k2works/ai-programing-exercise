open System

// ============================================================
// 型定義
// ============================================================

type Address = int
type Value = int

type CPU = {
    PC   : Address          // プログラムカウンタ
    ACC  : Value            // アキュムレータ
    Flag : bool             // ゼロフラグ
    Mem  : Map<Address, Value>  // メモリ
}

type Instruction =
    | NOP                   // 何もしない
    | LDA of Address        // メモリからロード
    | STA of Address        // メモリにストア
    | ADD of Address        // 足し算
    | SUB of Address        // 引き算
    | AND of Address        // 論理AND
    | OR  of Address        // 論理OR
    | NOT                   // ビット反転
    | JMP of Address        // 無条件ジャンプ
    | JZ  of Address        // ゼロならジャンプ
    | JNZ of Address        // ゼロでなければジャンプ
    | IN                    // 入力
    | OUT                   // 出力
    | SHL                   // 左シフト
    | SHR                   // 右シフト
    | HLT                   // 停止

// 実行結果（純粋な部分）
type StepResult =
    | Continue of CPU               // 続行
    | Halted of CPU                 // 停止
    | NeedInput of (Value -> CPU)   // 入力待ち
    | Output of Value * CPU         // 出力あり

// トレース（実行履歴）
type Trace = {
    Outputs : Value list
    Steps   : int
    Final   : CPU
}

// ============================================================
// 4ビットマスク
// ============================================================

let mask4 (x: int) : int = x &&& 0xF

// ============================================================
// CPU初期化
// ============================================================

let initCPU : CPU = {
    PC   = 0
    ACC  = 0
    Flag = false
    Mem  = Map.ofList [ for i in 0..15 -> (i, 0) ]
}

// メモリにプログラムをロード
let loadProgram (program: (Address * Value) list) (cpu: CPU) : CPU =
    let newMem = program |> List.fold (fun m (addr, value) -> Map.add addr value m) cpu.Mem
    { cpu with Mem = newMem }

// ============================================================
// 命令デコード
// ============================================================

let decode (byte: Value) : Instruction =
    let opcode  = (byte >>> 4) &&& 0xF
    let operand = byte &&& 0xF
    match opcode with
    | 0x0 -> NOP
    | 0x1 -> LDA operand
    | 0x2 -> STA operand
    | 0x3 -> ADD operand
    | 0x4 -> SUB operand
    | 0x5 -> AND operand
    | 0x6 -> OR  operand
    | 0x7 -> NOT
    | 0x8 -> JMP operand
    | 0x9 -> JZ  operand
    | 0xA -> JNZ operand
    | 0xB -> IN
    | 0xC -> OUT
    | 0xD -> SHL
    | 0xE -> SHR
    | 0xF -> HLT
    | _   -> NOP

// 命令エンコード（プログラム作成用）
let encode (instr: Instruction) : Value =
    match instr with
    | NOP      -> 0x00
    | LDA addr -> 0x10 + addr
    | STA addr -> 0x20 + addr
    | ADD addr -> 0x30 + addr
    | SUB addr -> 0x40 + addr
    | AND addr -> 0x50 + addr
    | OR  addr -> 0x60 + addr
    | NOT      -> 0x70
    | JMP addr -> 0x80 + addr
    | JZ  addr -> 0x90 + addr
    | JNZ addr -> 0xA0 + addr
    | IN       -> 0xB0
    | OUT      -> 0xC0
    | SHL      -> 0xD0
    | SHR      -> 0xE0
    | HLT      -> 0xF0

// ============================================================
// 1ステップ実行（純粋）
// ============================================================

let step (cpu: CPU) : StepResult =
    let instruction = decode (Map.find cpu.PC cpu.Mem)
    let cpu' = { cpu with PC = cpu.PC + 1 }

    // ACC更新とフラグ設定を同時に行う
    let updateAcc (newVal: Value) : CPU =
        let masked = mask4 newVal
        { cpu' with ACC = masked; Flag = (masked = 0) }

    // メモリから値を読む
    let readMem (addr: Address) : Value =
        mask4 (Map.find addr cpu.Mem)

    match instruction with
    | NOP -> Continue cpu'

    | LDA addr -> Continue (updateAcc (readMem addr))

    | STA addr -> Continue { cpu' with Mem = Map.add addr cpu.ACC cpu'.Mem }

    | ADD addr -> Continue (updateAcc (cpu.ACC + readMem addr))

    | SUB addr -> Continue (updateAcc (cpu.ACC - readMem addr))

    | AND addr -> Continue (updateAcc (cpu.ACC &&& readMem addr))

    | OR addr  -> Continue (updateAcc (cpu.ACC ||| readMem addr))

    | NOT -> Continue (updateAcc (~~~cpu.ACC))

    | JMP addr -> Continue { cpu' with PC = addr }

    | JZ addr ->
        Continue (if cpu.Flag then { cpu' with PC = addr } else cpu')

    | JNZ addr ->
        Continue (if not cpu.Flag then { cpu' with PC = addr } else cpu')

    | IN -> NeedInput (fun value -> updateAcc value)

    | OUT -> Output (cpu.ACC, cpu')

    | SHL -> Continue (updateAcc (cpu.ACC <<< 1))

    | SHR -> Continue (updateAcc (cpu.ACC >>> 1))

    | HLT -> Halted cpu

// ============================================================
// 純粋な実行（IOなし、トレース付き）
// ============================================================

let runPure (cpu: CPU) : Trace =
    let rec go outputs steps cpu =
        if cpu.PC < 0 || cpu.PC > 15 then
            { Outputs = outputs; Steps = steps; Final = cpu }
        elif steps > 1000 then
            // 無限ループ防止
            { Outputs = outputs; Steps = steps; Final = cpu }
        else
            match step cpu with
            | Continue cpu' ->
                go outputs (steps + 1) cpu'
            | Halted cpu' ->
                { Outputs = outputs; Steps = steps + 1; Final = cpu' }
            | NeedInput _ ->
                // 入力で停止
                { Outputs = outputs; Steps = steps; Final = cpu }
            | Output (value, cpu') ->
                go (outputs @ [value]) (steps + 1) cpu'

    go [] 0 cpu

// ============================================================
// 実行（IO版）
// ============================================================

let runCPU (cpu: CPU) : CPU =
    let rec loop cpu =
        if cpu.PC < 0 || cpu.PC > 15 then
            cpu
        else
            match step cpu with
            | Continue cpu' ->
                loop cpu'
            | Halted cpu' ->
                printfn "=== 停止 ==="
                cpu'
            | NeedInput f ->
                printf "入力 (0-15): "
                let value = Console.ReadLine() |> int |> mask4
                loop (f value)
            | Output (value, cpu') ->
                printfn "出力: %d" value
                loop cpu'

    loop cpu

// ============================================================
// サンプルプログラム
// ============================================================

// 3 + 5 = 8
let program1 : (Address * Value) list =
    [ (0,  encode (LDA 14))   // ACC <- mem[14] (3)
      (1,  encode (ADD 15))   // ACC <- ACC + mem[15] (5)
      (2,  encode OUT)        // 出力
      (3,  encode HLT)        // 停止
      (14, 3)                 // データ: 3
      (15, 5)                 // データ: 5
    ]

// カウントダウン: 5 -> 0
let program2 : (Address * Value) list =
    [ (0,  encode (LDA 15))   // ACC <- 5
      (1,  encode OUT)        // 出力
      (2,  encode (SUB 14))   // ACC <- ACC - 1
      (3,  encode (JZ 6))     // ゼロなら6番地へ
      (4,  encode (JMP 1))    // 1番地に戻る
      (5,  encode HLT)        // （ここには来ない）
      (6,  encode OUT)        // 最後の0を出力
      (7,  encode HLT)        // 停止
      (14, 1)                 // データ: 1
      (15, 5)                 // データ: 5
    ]

// 掛け算: 3 × 4 = 12（繰り返し足し算）
let program3 : (Address * Value) list =
    [ (0,  encode (LDA 15))   // ACC <- 0 (結果)
      (1,  encode (ADD 13))   // ACC <- ACC + 3
      (2,  encode (STA 15))   // 結果を保存
      (3,  encode (LDA 14))   // ACC <- カウンタ
      (4,  encode (SUB 12))   // カウンタ - 1
      (5,  encode (STA 14))   // カウンタを保存
      (6,  encode (JZ 9))     // ゼロなら終了へ
      (7,  encode (JMP 0))    // ループ
      (8,  encode HLT)        // （ここには来ない）
      (9,  encode (LDA 15))   // 結果をロード
      (10, encode OUT)        // 出力
      (11, encode HLT)        // 停止
      (12, 1)                 // データ: 1（減算用）
      (13, 3)                 // データ: 3（足す数）
      (14, 4)                 // データ: 4（カウンタ）
      (15, 0)                 // データ: 0（結果）
    ]

// ビット演算デモ: 0b1010 AND 0b1100 = 0b1000 (10 AND 12 = 8)
let program4 : (Address * Value) list =
    [ (0,  encode (LDA 14))   // ACC <- 10 (0b1010)
      (1,  encode (AND 15))   // ACC <- ACC AND 12 (0b1100)
      (2,  encode OUT)        // 出力: 8 (0b1000)
      (3,  encode HLT)        // 停止
      (14, 10)                // データ: 0b1010
      (15, 12)                // データ: 0b1100
    ]

// ============================================================
// テスト実行
// ============================================================

let test (name: string) (program: (Address * Value) list) : unit =
    printfn "\n=== %s ===" name
    let cpu = loadProgram program initCPU
    let trace = runPure cpu
    printfn "出力: %A" trace.Outputs
    printfn "ステップ数: %d" trace.Steps

let testAll () : unit =
    test "3 + 5 = 8" program1
    test "カウントダウン 5→0" program2
    test "3 × 4 = 12" program3
    test "10 AND 12 = 8" program4

// ============================================================
// メイン
// ============================================================

printfn "╔════════════════════════════════════╗"
printfn "║     4ビットCPUシミュレータ          ║"
printfn "╚════════════════════════════════════╝"
testAll ()
