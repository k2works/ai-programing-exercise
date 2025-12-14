// 4ビットCPUシミュレータ

type CPU = {
    mutable PC: int      // プログラムカウンタ
    mutable ACC: int     // アキュムレータ
    mutable Flag: bool   // ゼロフラグ
    Memory: int array    // メモリ（16アドレス）
}

// 命令コード
let NOP = 0b0000
let LDA = 0b0001
let STA = 0b0010
let ADD = 0b0011
let SUB = 0b0100
let AND = 0b0101
let OR  = 0b0110
let NOT = 0b0111
let JMP = 0b1000
let JZ  = 0b1001
let JNZ = 0b1010
let IN  = 0b1011
let OUT = 0b1100
let SHL = 0b1101
let SHR = 0b1110
let HLT = 0b1111

// 4ビットにマスク
let mask4bit value = value &&& 0b1111

// 命令名を取得
let getInstructionName opcode =
    match opcode with
    | 0b0000 -> "NOP" | 0b0001 -> "LDA" | 0b0010 -> "STA" | 0b0011 -> "ADD"
    | 0b0100 -> "SUB" | 0b0101 -> "AND" | 0b0110 -> "OR"  | 0b0111 -> "NOT"
    | 0b1000 -> "JMP" | 0b1001 -> "JZ"  | 0b1010 -> "JNZ" | 0b1011 -> "IN"
    | 0b1100 -> "OUT" | 0b1101 -> "SHL" | 0b1110 -> "SHR" | 0b1111 -> "HLT"
    | _ -> "???"

// CPU初期化
let createCPU () = {
    PC = 0
    ACC = 0
    Flag = false
    Memory = Array.zeroCreate 16
}

// 1命令実行（デバッグモード対応）
let step debug cpu =
    let currentPC = cpu.PC
    let instruction = cpu.Memory.[cpu.PC]
    let opcode = (instruction >>> 4) &&& 0b1111  // 上位4ビット
    let operand = instruction &&& 0b1111         // 下位4ビット

    // デバッグ: 実行前の状態
    if debug then
        printfn "────────────────────────────────────"
        printfn "PC=%d  命令=%s %d  (機械語: %08B)" currentPC (getInstructionName opcode) operand instruction
        printfn "  実行前: ACC=%d  Flag=%b" cpu.ACC cpu.Flag

    cpu.PC <- cpu.PC + 1

    match opcode with
    | op when op = NOP -> ()
    | op when op = LDA ->
        cpu.ACC <- cpu.Memory.[operand] &&& 0b1111
        cpu.Flag <- (cpu.ACC = 0)
    | op when op = STA ->
        cpu.Memory.[operand] <- cpu.ACC
    | op when op = ADD ->
        cpu.ACC <- mask4bit (cpu.ACC + (cpu.Memory.[operand] &&& 0b1111))
        cpu.Flag <- (cpu.ACC = 0)
    | op when op = SUB ->
        cpu.ACC <- mask4bit (cpu.ACC - (cpu.Memory.[operand] &&& 0b1111))
        cpu.Flag <- (cpu.ACC = 0)
    | op when op = AND ->
        cpu.ACC <- cpu.ACC &&& (cpu.Memory.[operand] &&& 0b1111)
        cpu.Flag <- (cpu.ACC = 0)
    | op when op = OR ->
        cpu.ACC <- cpu.ACC ||| (cpu.Memory.[operand] &&& 0b1111)
        cpu.Flag <- (cpu.ACC = 0)
    | op when op = NOT ->
        cpu.ACC <- mask4bit (~~~cpu.ACC)
        cpu.Flag <- (cpu.ACC = 0)
    | op when op = JMP ->
        cpu.PC <- operand
    | op when op = JZ ->
        if cpu.Flag then cpu.PC <- operand
    | op when op = JNZ ->
        if not cpu.Flag then cpu.PC <- operand
    | op when op = IN ->
        printfn "入力してください (0-15): "
        cpu.ACC <- System.Console.ReadLine() |> int |> mask4bit
    | op when op = OUT ->
        printfn "出力: %d" cpu.ACC
    | op when op = SHL ->
        cpu.ACC <- mask4bit (cpu.ACC <<< 1)
        cpu.Flag <- (cpu.ACC = 0)
    | op when op = SHR ->
        cpu.ACC <- cpu.ACC >>> 1
        cpu.Flag <- (cpu.ACC = 0)
    | op when op = HLT ->
        cpu.PC <- -1  // 停止のサイン
    | _ -> ()

    // デバッグ: 実行後の状態
    if debug then
        printfn "  実行後: ACC=%d  Flag=%b  次PC=%d" cpu.ACC cpu.Flag cpu.PC

    opcode <> HLT  // HLTならfalseを返す

// 実行（通常モード）
let run cpu =
    while cpu.PC >= 0 && cpu.PC < 16 && step false cpu do
        ()

// 実行（デバッグモード）
let runDebug cpu =
    printfn "\n╔════════════════════════════════════╗"
    printfn "║        デバッグモード開始          ║"
    printfn "╚════════════════════════════════════╝"
    while cpu.PC >= 0 && cpu.PC < 16 && step true cpu do
        ()
    printfn "────────────────────────────────────"
    printfn "実行完了\n"

// テスト: 3 + 5 = 8
let test1 () =
    let cpu = createCPU()
    // プログラム
    cpu.Memory.[0] <- 0b0001_1110  // LDA 14
    cpu.Memory.[1] <- 0b0011_1111  // ADD 15
    cpu.Memory.[2] <- 0b1100_0000  // OUT
    cpu.Memory.[3] <- 0b1111_0000  // HLT
    // データ
    cpu.Memory.[14] <- 3
    cpu.Memory.[15] <- 5

    printfn "=== 3 + 5 を計算（デバッグモード）==="
    runDebug cpu

test1 ()
