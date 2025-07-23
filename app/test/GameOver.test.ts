import { describe, it, expect, beforeEach, vi } from 'vitest'
import { Game } from '../src/Game'

describe('Game Over System', () => {
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

  describe('Game Over Detection', () => {
    it('should not trigger game over when stage is empty', () => {
      game.start()
      
      const isGameOver = game.checkGameOver()
      expect(isGameOver).toBe(false)
    })

    it('should trigger game over when new puyo cannot be placed', () => {
      game.start()
      const stage = game.getStage()
      
      // ステージ上部を埋める（新しいぷよが配置できない状態）
      const startX = Math.floor(stage.getWidth() / 2)
      stage.setCell(startX, 0, 1) // スタート位置を塞ぐ
      stage.setCell(startX, 1, 1)
      
      const isGameOver = game.checkGameOver()
      expect(isGameOver).toBe(true)
    })

    it('should detect game over based on spawn position occupation', () => {
      game.start()
      const stage = game.getStage()
      
      // ぷよの生成位置付近を埋める
      stage.setCell(2, 1, 1) // メイン位置
      stage.setCell(2, 0, 2) // サブ位置
      
      const isGameOver = game.checkGameOver()
      expect(isGameOver).toBe(true)
    })

    it('should not trigger game over when only lower areas are filled', () => {
      game.start()
      const stage = game.getStage()
      
      // 下の方だけを埋める
      for (let x = 0; x < stage.getWidth(); x++) {
        stage.setCell(x, 10, 1)
        stage.setCell(x, 11, 2)
      }
      
      const isGameOver = game.checkGameOver()
      expect(isGameOver).toBe(false)
    })
  })

  describe('Game Over State Management', () => {
    it('should change game running state when game over occurs', () => {
      game.start()
      expect(game.isRunning()).toBe(true)
      
      // ゲームオーバー状態を作成
      const stage = game.getStage()
      stage.setCell(2, 1, 1)
      
      game.handleGameOver()
      expect(game.isRunning()).toBe(false)
    })

    it('should provide game over information', () => {
      game.start()
      
      const gameOverInfo = game.getGameOverInfo()
      
      expect(gameOverInfo).toHaveProperty('isGameOver')
      expect(gameOverInfo).toHaveProperty('finalScore')
      expect(gameOverInfo).toHaveProperty('allClearCount')
      expect(gameOverInfo).toHaveProperty('playTime')
    })

    it('should track game statistics until game over', () => {
      game.start()
      
      // スコアを増やす
      const stage = game.getStage()
      stage.setCell(0, 11, 1)
      stage.setCell(1, 11, 1)
      stage.setCell(2, 11, 1)
      stage.setCell(3, 11, 1)
      
      game.processEliminationWithAllClearCheck()
      
      // ゲームオーバーに至る
      stage.setCell(2, 1, 1)
      game.handleGameOver()
      
      const info = game.getGameOverInfo()
      expect(info.finalScore).toBeGreaterThan(0)
      expect(info.allClearCount).toBe(1)
    })
  })

  describe('Game Over Animation and Effects', () => {
    it('should provide game over effect information', () => {
      const effectInfo = game.getGameOverEffect()
      
      expect(effectInfo).toHaveProperty('message')
      expect(effectInfo).toHaveProperty('duration')
      expect(effectInfo).toHaveProperty('color')
      expect(effectInfo).toHaveProperty('animation')
      
      expect(effectInfo.message).toContain('ゲームオーバー')
      expect(effectInfo.duration).toBeGreaterThan(0)
    })

    it('should have different effects based on performance', () => {
      game.start()
      
      // 高スコアの場合
      const highScoreEffect = game.getGameOverEffect(50000)
      expect(highScoreEffect.message).toContain('Great!')
      
      // 低スコアの場合
      const lowScoreEffect = game.getGameOverEffect(100)
      expect(lowScoreEffect.message).toContain('ゲームオーバー')
    })

    it('should provide game over sound information', () => {
      const soundInfo = game.getGameOverSound()
      
      expect(soundInfo).toHaveProperty('soundType')
      expect(soundInfo).toHaveProperty('volume')
      expect(soundInfo).toHaveProperty('duration')
      
      expect(['game_over', 'excellent'].includes(soundInfo.soundType)).toBe(true)
    })
  })

  describe('Restart Functionality', () => {
    it('should restart game properly after game over', () => {
      game.start()
      const stage = game.getStage()
      
      // ゲームオーバー状態を作成
      stage.setCell(2, 1, 1)
      game.handleGameOver()
      expect(game.isRunning()).toBe(false)
      
      // リスタート
      game.restart()
      
      expect(game.isRunning()).toBe(true)
      expect(game.getScore()).toBe(0)
      expect(game.getAllClearCount()).toBe(0)
      expect(game.getStage().isEmpty()).toBe(true)
    })

    it('should reset all game statistics on restart', () => {
      game.start()
      
      // スコアと統計を増やす
      const stage = game.getStage()
      stage.setCell(0, 11, 1)
      stage.setCell(1, 11, 1)
      stage.setCell(2, 11, 1)
      stage.setCell(3, 11, 1)
      
      game.processEliminationWithAllClearCheck()
      const beforeScore = game.getScore()
      const beforeAllClear = game.getAllClearCount()
      
      expect(beforeScore).toBeGreaterThan(0)
      expect(beforeAllClear).toBe(1)
      
      // リスタート
      game.restart()
      
      expect(game.getScore()).toBe(0)
      expect(game.getAllClearCount()).toBe(0)
    })

    it('should generate new puyo after restart', () => {
      game.start()
      
      // ゲームオーバー
      const stage = game.getStage()
      stage.setCell(2, 1, 1)
      game.handleGameOver()
      
      // リスタート
      game.restart()
      
      const currentPuyo = game.getCurrentPuyo()
      expect(currentPuyo).not.toBe(null)
      expect(currentPuyo?.main.color).toBeGreaterThan(0)
      expect(currentPuyo?.sub.color).toBeGreaterThan(0)
    })
  })

  describe('Game Over Integration with Other Systems', () => {
    it('should check game over after puyo placement', () => {
      game.start()
      const stage = game.getStage()
      
      // ステージをほぼ満杯にする
      for (let y = 2; y < stage.getHeight(); y++) {
        for (let x = 0; x < stage.getWidth(); x++) {
          if (x !== 2 || y !== 2) { // 一部だけ空けておく
            stage.setCell(x, y, (x % 4) + 1)
          }
        }
      }
      
      // 現在のぷよを危険な位置に配置
      const currentPuyo = game.getCurrentPuyo()
      if (currentPuyo) {
        currentPuyo.main.x = 2
        currentPuyo.main.y = 2
        currentPuyo.sub.x = 2
        currentPuyo.sub.y = 1
      }
      
      // ぷよ固定と同時にゲームオーバーチェック
      const wasGameOver = game.fixCurrentPuyoWithGameOverCheck()
      
      expect(wasGameOver).toBe(true)
      expect(game.isRunning()).toBe(false)
    })

    it('should not continue generating puyo after game over', () => {
      game.start()
      
      // ゲームオーバー状態を作成
      const stage = game.getStage()
      stage.setCell(2, 1, 1)
      game.handleGameOver()
      
      expect(game.isRunning()).toBe(false)
      
      // 新しいぷよの生成を試行
      game.update()
      
      // ゲームオーバー後は新しいぷよが生成されない
      expect(game.isRunning()).toBe(false)
    })

    it('should calculate final statistics correctly', () => {
      game.start()
      
      // 複数回の消去と全消しを実行
      const stage = game.getStage()
      
      // 1回目の全消し
      stage.setCell(0, 11, 1)
      stage.setCell(1, 11, 1)
      stage.setCell(2, 11, 1)
      stage.setCell(3, 11, 1)
      game.processEliminationWithAllClearCheck()
      
      // スコア追加
      stage.setCell(0, 11, 2)
      stage.setCell(1, 11, 2)
      stage.setCell(2, 11, 2)
      stage.setCell(3, 11, 2)
      game.processEliminationWithAllClearCheck()
      
      // ゲームオーバー
      stage.setCell(2, 1, 1)
      game.handleGameOver()
      
      const finalStats = game.getGameOverInfo()
      expect(finalStats.finalScore).toBeGreaterThan(0)
      expect(finalStats.allClearCount).toBe(2)
      expect(finalStats.isGameOver).toBe(true)
    })
  })

  describe('Edge Cases', () => {
    it('should handle game over immediately at game start if stage is blocked', () => {
      game.start()
      const stage = game.getStage()
      
      // 開始前にスタート位置を塞ぐ
      stage.setCell(2, 1, 1)
      stage.setCell(2, 0, 2)
      
      const isGameOver = game.checkGameOver()
      expect(isGameOver).toBe(true)
      
      game.handleGameOver()
      expect(game.isRunning()).toBe(false)
    })

    it('should handle multiple restart calls', () => {
      game.start()
      const stage = game.getStage()
      
      // ゲームオーバー
      stage.setCell(2, 1, 1)
      game.handleGameOver()
      
      // 複数回のリスタート
      game.restart()
      expect(game.isRunning()).toBe(true)
      
      game.restart() // 2回目
      expect(game.isRunning()).toBe(true)
      expect(game.getScore()).toBe(0)
    })

    it('should maintain game over state until explicit restart', () => {
      game.start()
      const stage = game.getStage()
      
      // ゲームオーバー
      stage.setCell(2, 1, 1)
      game.handleGameOver()
      
      expect(game.isRunning()).toBe(false)
      
      // updateを呼んでもゲームオーバー状態は維持される
      game.update()
      game.update()
      game.update()
      
      expect(game.isRunning()).toBe(false)
    })
  })
})