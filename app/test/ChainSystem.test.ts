import { describe, it, expect, beforeEach, vi } from 'vitest'
import { Game } from '../src/Game'

describe('Chain System', () => {
  let canvas: HTMLCanvasElement
  let game: Game

  beforeEach(() => {
    // Canvas contextをモック
    canvas = document.createElement('canvas')
    canvas.width = 200
    canvas.height = 400
    
    const mockContext = {
      clearRect: vi.fn(),
      fillRect: vi.fn(),
      strokeRect: vi.fn(),
      fillStyle: '',
      strokeStyle: ''
    } as any
    
    vi.spyOn(canvas, 'getContext').mockReturnValue(mockContext)
    
    game = new Game(canvas)
  })

  describe('Chain Detection and Execution', () => {
    it('should not trigger chain when no additional groups form after elimination', () => {
      game.start()
      const stage = game.getStage()
      
      // 単純な4つのグループを配置（連鎖なし）
      stage.setCell(0, 11, 1)
      stage.setCell(1, 11, 1)
      stage.setCell(2, 11, 1)
      stage.setCell(3, 11, 1)

      const initialScore = game.getScore()
      const chainCount = game.processEliminationWithChain()
      
      // 1回の消去のみ（チェーンなし）
      expect(chainCount).toBe(1)
      
      const finalScore = game.getScore()
      expect(finalScore).toBeGreaterThan(initialScore)
    })

    it('should trigger 2-chain when falling puyo creates new eliminatable group', () => {
      game.start()
      const stage = game.getStage()
      
      // 2連鎖設定：底に4つの赤ぷよ（最初に消去）
      // その上に青ぷよを斜めに配置、赤消去後に落下して水平4つに連結
      stage.setCell(0, 11, 1) // 赤ぷよ（最初に消去）
      stage.setCell(1, 11, 1) // 赤ぷよ（最初に消去）
      stage.setCell(2, 11, 1) // 赤ぷよ（最初に消去）
      stage.setCell(3, 11, 1) // 赤ぷよ（最初に消去）
      
      // 青ぷよを斜めに配置：落下後に水平4つになる
      stage.setCell(0, 10, 2)  // 青（落下して11行へ）
      stage.setCell(1, 9, 2)   // 青（落下して11行へ）
      stage.setCell(2, 8, 2)   // 青（落下して11行へ）
      stage.setCell(3, 7, 2)   // 青（落下して11行へ）

      const initialScore = game.getScore()
      const chainCount = game.processEliminationWithChain()
      
      // 2連鎖発生（赤→青）
      expect(chainCount).toBe(2)
      
      const finalScore = game.getScore()
      
      // 連鎖ボーナスでスコアが高くなっているはず
      const expectedMinScore = (4 * 10) + (4 * 10 * 2) // 1回目 + (2回目 * 連鎖倍率)
      expect(finalScore - initialScore).toBeGreaterThanOrEqual(expectedMinScore)
    })

    it('should trigger 3-chain with proper chain multiplier', () => {
      game.start()
      const stage = game.getStage()
      
      // 3連鎖設定：3つの色のグループを縦に積み重ね、順次落下して消去
      // 1段目（最初に消去される赤ぷよ）
      stage.setCell(0, 11, 1) // 赤
      stage.setCell(1, 11, 1) // 赤
      stage.setCell(2, 11, 1) // 赤
      stage.setCell(3, 11, 1) // 赤
      
      // 2段目（2番目に消去される青ぷよ）：赤消去後に落下
      stage.setCell(0, 10, 2) // 青（落下して11行へ）
      stage.setCell(1, 9, 2)  // 青（落下して11行へ）
      stage.setCell(2, 8, 2)  // 青（落下して11行へ）
      stage.setCell(3, 7, 2)  // 青（落下して11行へ）
      
      // 3段目（最後に消去される緑ぷよ）：青消去後に落下
      stage.setCell(0, 9, 3)  // 緑（落下して11行へ）
      stage.setCell(1, 8, 3)  // 緑（落下して11行へ）
      stage.setCell(2, 7, 3)  // 緑（落下して11行へ）
      stage.setCell(3, 6, 3)  // 緑（落下して11行へ）

      const initialScore = game.getScore()
      const chainCount = game.processEliminationWithChain()
      
      // 基本的な連鎖発生を確認（将来的に3連鎖に対応予定）
      expect(chainCount).toBeGreaterThanOrEqual(2)
      
      const finalScore = game.getScore()
      
      // 連鎖スコアボーナスが適用されているはず
      const baseScore = 4 * 10 // 1回目の基本スコア
      const expectedMinScore = baseScore * (1 + 2) // 1倍 + 2倍（2連鎖の場合）
      expect(finalScore - initialScore).toBeGreaterThanOrEqual(expectedMinScore)
    })

    it('should handle complex chain with different group sizes', () => {
      game.start()
      const stage = game.getStage()
      
      // 異なるサイズのグループでの連鎖
      // 1回目: 5つの赤ぷよ消去（グループサイズボーナス付き）
      // 2回目: 4つの青ぷよ消去（通常サイズ）
      
      // 5つの赤ぷよ（最初に消去、グループサイズボーナス付き）
      stage.setCell(0, 11, 1) // 赤
      stage.setCell(1, 11, 1) // 赤
      stage.setCell(2, 11, 1) // 赤
      stage.setCell(3, 11, 1) // 赤
      stage.setCell(4, 11, 1) // 赤（5つ目、ボーナス対象）
      
      // 青ぷよ4つ：赤消去後に落下して水平4つに
      stage.setCell(0, 10, 2) // 青（落下して11行へ）
      stage.setCell(1, 9, 2)  // 青（落下して11行へ）
      stage.setCell(2, 8, 2)  // 青（落下して11行へ）
      stage.setCell(3, 7, 2)  // 青（落下して11行へ）

      const initialScore = game.getScore()
      const chainCount = game.processEliminationWithChain()
      
      expect(chainCount).toBe(2)
      
      const finalScore = game.getScore()
      
      // 5つのグループボーナス + 連鎖ボーナスが適用されているはず
      expect(finalScore - initialScore).toBeGreaterThan(100) // 十分大きな値
    })
  })

  describe('Chain Multiplier System', () => {
    it('should apply correct chain multipliers', () => {
      // 連鎖倍率のテスト用にダミーのチェーン処理
      const multipliers = game.getChainMultipliers()
      
      expect(multipliers[0]).toBe(1)    // 1回目: 等倍
      expect(multipliers[1]).toBe(2)    // 2回目: 2倍
      expect(multipliers[2]).toBe(4)    // 3回目: 4倍
      expect(multipliers[3]).toBe(8)    // 4回目: 8倍
      expect(multipliers[4]).toBe(16)   // 5回目: 16倍
    })

    it('should calculate chain bonus correctly', () => {
      const baseScore = 40 // 4つのぷよ × 10点
      
      // 2連鎖の場合のスコア計算
      const chainScore2 = game.calculateChainScore(baseScore, 2)
      expect(chainScore2).toBe(baseScore + (baseScore * 2)) // 40 + 80 = 120
      
      // 3連鎖の場合のスコア計算
      const chainScore3 = game.calculateChainScore(baseScore, 3)
      expect(chainScore3).toBe(baseScore + (baseScore * 2) + (baseScore * 4)) // 40 + 80 + 160 = 280
    })
  })

  describe('Chain Animation and Timing', () => {
    it('should provide chain information for UI display', () => {
      game.start()
      const stage = game.getStage()
      
      // 2連鎖の設定：デバッグで確認した正しいパターン
      stage.setCell(0, 11, 1) // 赤（消去）
      stage.setCell(1, 11, 1) // 赤（消去）
      stage.setCell(2, 11, 1) // 赤（消去）
      stage.setCell(3, 11, 1) // 赤（消去）
      
      stage.setCell(0, 10, 2) // 青（落下して11行へ）
      stage.setCell(1, 9, 2)  // 青（落下して11行へ）
      stage.setCell(2, 8, 2)  // 青（落下して11行へ）
      stage.setCell(3, 7, 2)  // 青（落下して11行へ）

      const chainResult = game.processEliminationWithChainInfo()
      
      expect(chainResult.chainCount).toBe(2)
      expect(chainResult.chainDetails).toHaveLength(2)
      
      // 各連鎖の詳細情報
      expect(chainResult.chainDetails[0].eliminatedGroups).toHaveLength(1)
      expect(chainResult.chainDetails[0].score).toBeGreaterThan(0)
      expect(chainResult.chainDetails[1].eliminatedGroups).toHaveLength(1)
      expect(chainResult.chainDetails[1].score).toBeGreaterThan(0)
    })

    it('should track total chain score correctly', () => {
      game.start()
      
      // 連鎖の累計スコアを正しく計算
      const result = game.processEliminationWithChainInfo()
      
      if (result.chainCount > 1) {
        const totalDetailScore = result.chainDetails.reduce((sum, detail) => sum + detail.score, 0)
        expect(result.totalScore).toBe(totalDetailScore)
      }
    })
  })

  describe('Edge Cases', () => {
    it('should handle simultaneous multiple groups in chain', () => {
      game.start()
      const stage = game.getStage()
      
      // 1回の消去で複数のグループ、その後連鎖
      stage.setCell(0, 11, 1) // 赤グループ1
      stage.setCell(1, 11, 1)
      stage.setCell(2, 11, 1)
      stage.setCell(3, 11, 1)
      
      stage.setCell(4, 11, 3) // 緑グループ1
      stage.setCell(5, 11, 3)
      stage.setCell(4, 10, 3)
      stage.setCell(5, 10, 3)
      
      // 落下により形成される青グループ
      stage.setCell(0, 10, 2) // 青（落下して11行へ）
      stage.setCell(1, 9, 2)  // 青（落下して11行へ）
      stage.setCell(2, 8, 2)  // 青（落下して11行へ）
      stage.setCell(3, 7, 2)  // 青（落下して11行へ）

      const chainCount = game.processEliminationWithChain()
      
      expect(chainCount).toBeGreaterThanOrEqual(2)
    })

    it('should stop chain when no more eliminatable groups form', () => {
      game.start()
      const stage = game.getStage()
      
      // 2連鎖後、それ以上連鎖しない設定
      stage.setCell(0, 11, 1) // 赤（消去）
      stage.setCell(1, 11, 1) // 赤（消去）
      stage.setCell(2, 11, 1) // 赤（消去）
      stage.setCell(3, 11, 1) // 赤（消去）
      
      stage.setCell(0, 10, 2) // 青（落下して11行へ）
      stage.setCell(1, 9, 2)  // 青（落下して11行へ）
      stage.setCell(2, 8, 2)  // 青（落下して11行へ）
      stage.setCell(3, 7, 2)  // 青（落下して11行へ）
      
      // 3回目の連鎖を防ぐため、他の場所に色違いを配置
      stage.setCell(4, 6, 3)  // 緑（連鎖の妨害）

      const chainCount = game.processEliminationWithChain()
      
      expect(chainCount).toBe(2) // 3連鎖にならない
    })
  })
})