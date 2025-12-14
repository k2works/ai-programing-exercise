module FourBitCPU where

import Data.Bits ((.&.), (.|.), complement, shiftL, shiftR)
import Data.Array (Array, array, (//), (!))
import Control.Monad (when)

-- ============================================================
-- 型定義
-- ============================================================

type Address = Int
type Value = Int

data CPU = CPU
    { pc   :: Address    -- プログラムカウンタ
    , acc  :: Value      -- アキュムレータ
    , flag :: Bool       -- ゼロフラグ
    , mem  :: Array Address Value  -- メモリ
    } deriving (Show)

data Instruction
    = NOP           -- 何もしない
    | LDA Address   -- メモリからロード
    | STA Address   -- メモリにストア
    | ADD Address   -- 足し算
    | SUB Address   -- 引き算
    | AND Address   -- 論理AND
    | OR  Address   -- 論理OR
    | NOT           -- ビット反転
    | JMP Address   -- 無条件ジャンプ
    | JZ  Address   -- ゼロならジャンプ
    | JNZ Address   -- ゼロでなければジャンプ
    | IN            -- 入力
    | OUT           -- 出力
    | SHL           -- 左シフト
    | SHR           -- 右シフト
    | HLT           -- 停止
    deriving (Show, Eq)

-- 実行結果（純粋な部分）
data StepResult
    = Continue CPU              -- 続行
    | Halted CPU                -- 停止
    | NeedInput (Value -> CPU)  -- 入力待ち
    | Output Value CPU          -- 出力あり

-- ============================================================
-- 4ビットマスク
-- ============================================================

mask4 :: Int -> Int
mask4 x = x .&. 0xF

-- ============================================================
-- CPU初期化
-- ============================================================

initCPU :: CPU
initCPU = CPU
    { pc   = 0
    , acc  = 0
    , flag = False
    , mem  = array (0, 15) [(i, 0) | i <- [0..15]]
    }

-- メモリにプログラムをロード
loadProgram :: [(Address, Value)] -> CPU -> CPU
loadProgram program cpu = cpu { mem = mem cpu // program }

-- ============================================================
-- 命令デコード
-- ============================================================

decode :: Value -> Instruction
decode byte =
    let opcode  = (byte `shiftR` 4) .&. 0xF
        operand = byte .&. 0xF
    in case opcode of
        0x0 -> NOP
        0x1 -> LDA operand
        0x2 -> STA operand
        0x3 -> ADD operand
        0x4 -> SUB operand
        0x5 -> AND operand
        0x6 -> OR  operand
        0x7 -> NOT
        0x8 -> JMP operand
        0x9 -> JZ  operand
        0xA -> JNZ operand
        0xB -> IN
        0xC -> OUT
        0xD -> SHL
        0xE -> SHR
        0xF -> HLT
        _   -> NOP

-- 命令エンコード（プログラム作成用）
encode :: Instruction -> Value
encode instr = case instr of
    NOP       -> 0x00
    LDA addr  -> 0x10 + addr
    STA addr  -> 0x20 + addr
    ADD addr  -> 0x30 + addr
    SUB addr  -> 0x40 + addr
    AND addr  -> 0x50 + addr
    OR  addr  -> 0x60 + addr
    NOT       -> 0x70
    JMP addr  -> 0x80 + addr
    JZ  addr  -> 0x90 + addr
    JNZ addr  -> 0xA0 + addr
    IN        -> 0xB0
    OUT       -> 0xC0
    SHL       -> 0xD0
    SHR       -> 0xE0
    HLT       -> 0xF0

-- ============================================================
-- 1ステップ実行（純粋）
-- ============================================================

step :: CPU -> StepResult
step cpu =
    let instruction = decode (mem cpu ! pc cpu)
        cpu' = cpu { pc = pc cpu + 1 }

        -- ACC更新とフラグ設定を同時に行う
        updateAcc :: Value -> CPU
        updateAcc newVal =
            let masked = mask4 newVal
            in cpu' { acc = masked, flag = masked == 0 }

        -- メモリから値を読む
        readMem :: Address -> Value
        readMem addr = mask4 (mem cpu ! addr)

    in case instruction of
        NOP -> Continue cpu'

        LDA addr -> Continue $ updateAcc (readMem addr)

        STA addr -> Continue $ cpu' { mem = mem cpu' // [(addr, acc cpu)] }

        ADD addr -> Continue $ updateAcc (acc cpu + readMem addr)

        SUB addr -> Continue $ updateAcc (acc cpu - readMem addr)

        AND addr -> Continue $ updateAcc (acc cpu .&. readMem addr)

        OR addr  -> Continue $ updateAcc (acc cpu .|. readMem addr)

        NOT -> Continue $ updateAcc (complement (acc cpu))

        JMP addr -> Continue $ cpu' { pc = addr }

        JZ addr -> Continue $
            if flag cpu then cpu' { pc = addr } else cpu'

        JNZ addr -> Continue $
            if not (flag cpu) then cpu' { pc = addr } else cpu'

        IN -> NeedInput (\val -> updateAcc val)

        OUT -> Output (acc cpu) cpu'

        SHL -> Continue $ updateAcc (acc cpu `shiftL` 1)

        SHR -> Continue $ updateAcc (acc cpu `shiftR` 1)

        HLT -> Halted cpu

-- ============================================================
-- 実行（IO版）
-- ============================================================

runCPU :: CPU -> IO CPU
runCPU cpu
    | pc cpu < 0 || pc cpu > 15 = return cpu
    | otherwise = case step cpu of
        Continue cpu' -> runCPU cpu'
        Halted cpu'   -> do
            putStrLn "=== 停止 ==="
            return cpu'
        NeedInput f   -> do
            putStr "入力 (0-15): "
            val <- readLn
            runCPU (f (mask4 val))
        Output val cpu' -> do
            putStrLn $ "出力: " ++ show val
            runCPU cpu'

-- ============================================================
-- 純粋な実行（IOなし、トレース付き）
-- ============================================================

data Trace = Trace
    { traceOutputs :: [Value]
    , traceSteps   :: Int
    , traceFinal   :: CPU
    } deriving (Show)

runPure :: CPU -> Trace
runPure = go [] 0
  where
    go outputs steps cpu
        | pc cpu < 0 || pc cpu > 15 = Trace outputs steps cpu
        | steps > 1000 = Trace outputs steps cpu  -- 無限ループ防止
        | otherwise = case step cpu of
            Continue cpu' -> go outputs (steps + 1) cpu'
            Halted cpu'   -> Trace outputs (steps + 1) cpu'
            NeedInput _   -> Trace outputs steps cpu  -- 入力で停止
            Output val cpu' -> go (outputs ++ [val]) (steps + 1) cpu'

-- ============================================================
-- サンプルプログラム
-- ============================================================

-- 3 + 5 = 8
program1 :: [(Address, Value)]
program1 =
    [ (0,  encode (LDA 14))   -- ACC <- mem[14] (3)
    , (1,  encode (ADD 15))   -- ACC <- ACC + mem[15] (5)
    , (2,  encode OUT)        -- 出力
    , (3,  encode HLT)        -- 停止
    , (14, 3)                 -- データ: 3
    , (15, 5)                 -- データ: 5
    ]

-- カウントダウン: 5 -> 0
program2 :: [(Address, Value)]
program2 =
    [ (0,  encode (LDA 15))   -- ACC <- 5
    , (1,  encode OUT)        -- 出力
    , (2,  encode (SUB 14))   -- ACC <- ACC - 1
    , (3,  encode (JZ 6))     -- ゼロなら6番地へ
    , (4,  encode (JMP 1))    -- 1番地に戻る
    , (5,  encode HLT)        -- （ここには来ない）
    , (6,  encode OUT)        -- 最後の0を出力
    , (7,  encode HLT)        -- 停止
    , (14, 1)                 -- データ: 1
    , (15, 5)                 -- データ: 5
    ]

-- 掛け算: 3 × 4 = 12（繰り返し足し算）
program3 :: [(Address, Value)]
program3 =
    [ (0,  encode (LDA 15))   -- ACC <- 0 (結果)
    , (1,  encode (ADD 13))   -- ACC <- ACC + 3
    , (2,  encode (STA 15))   -- 結果を保存
    , (3,  encode (LDA 14))   -- ACC <- カウンタ
    , (4,  encode (SUB 12))   -- カウンタ - 1
    , (5,  encode (STA 14))   -- カウンタを保存
    , (6,  encode (JZ 9))     -- ゼロなら終了へ
    , (7,  encode (JMP 0))    -- ループ
    , (8,  encode HLT)        -- （ここには来ない）
    , (9,  encode (LDA 15))   -- 結果をロード
    , (10, encode OUT)        -- 出力
    , (11, encode HLT)        -- 停止
    , (12, 1)                 -- データ: 1（減算用）
    , (13, 3)                 -- データ: 3（足す数）
    , (14, 4)                 -- データ: 4（カウンタ）
    , (15, 0)                 -- データ: 0（結果）
    ]

-- ビット演算デモ: 0b1010 AND 0b1100 = 0b1000 (10 AND 12 = 8)
program4 :: [(Address, Value)]
program4 =
    [ (0,  encode (LDA 14))   -- ACC <- 10 (0b1010)
    , (1,  encode (AND 15))   -- ACC <- ACC AND 12 (0b1100)
    , (2,  encode OUT)        -- 出力: 8 (0b1000)
    , (3,  encode HLT)        -- 停止
    , (14, 10)                -- データ: 0b1010
    , (15, 12)                -- データ: 0b1100
    ]

-- ============================================================
-- テスト実行
-- ============================================================

test :: String -> [(Address, Value)] -> IO ()
test name program = do
    putStrLn $ "\n=== " ++ name ++ " ==="
    let cpu = loadProgram program initCPU
    let trace = runPure cpu
    putStrLn $ "出力: " ++ show (traceOutputs trace)
    putStrLn $ "ステップ数: " ++ show (traceSteps trace)

testAll :: IO ()
testAll = do
    test "3 + 5 = 8" program1
    test "カウントダウン 5→0" program2
    test "3 × 4 = 12" program3
    test "10 AND 12 = 8" program4

-- ============================================================
-- メイン
-- ============================================================

main :: IO ()
main = do
    putStrLn "╔════════════════════════════════════╗"
    putStrLn "║     4ビットCPUシミュレータ          ║"
    putStrLn "╚════════════════════════════════════╝"
    testAll

    -- 対話モードで実行したい場合
    putStrLn "\n=== 対話モード: 3 + 5 ==="
    _ <- runCPU (loadProgram program1 initCPU)
    return ()
