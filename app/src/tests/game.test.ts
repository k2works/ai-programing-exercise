import { describe, it, expect, beforeEach, vi } from 'vitest'
import { Game } from '../game'

describe('Game', () => {
  let canvas: HTMLCanvasElement
  let scoreDisplay: HTMLElement
  let game: Game
  let mockContext: CanvasRenderingContext2D

  beforeEach(() => {
    // Canvas 2Dコンテキストのモック
    mockContext = {
      fillStyle: '',
      strokeStyle: '',
      fillRect: vi.fn(),
      strokeRect: vi.fn(),
      clearRect: vi.fn(),
      drawImage: vi.fn(),
    } as unknown as CanvasRenderingContext2D

    // Canvas要素をモック
    canvas = document.createElement('canvas')
    canvas.width = 320
    canvas.height = 480

    // getContextメソッドをモック
    vi.spyOn(canvas, 'getContext').mockReturnValue(mockContext)

    // スコア表示要素をモック
    scoreDisplay = document.createElement('div')

    game = new Game(canvas, scoreDisplay)
  })

  describe('初期化', () => {
    it('ゲームインスタンスが作成できる', () => {
      expect(game).toBeInstanceOf(Game)
    })

    it('初期スコアは0である', () => {
      expect(game.getScore()).toBe(0)
    })

    it('初期状態では実行中ではない', () => {
      expect(game.isGameRunning()).toBe(false)
    })
  })

  describe('ゲーム開始', () => {
    it('start()を呼ぶとゲームが開始される', () => {
      game.start()
      expect(game.isGameRunning()).toBe(true)
      game.stop() // テスト後のクリーンアップ
    })

    it('start()を複数回呼んでも問題ない', () => {
      game.start()
      game.start()
      expect(game.isGameRunning()).toBe(true)
      game.stop() // テスト後のクリーンアップ
    })
  })

  describe('ゲーム停止', () => {
    it('stop()を呼ぶとゲームが停止される', () => {
      game.start()
      game.stop()
      expect(game.isGameRunning()).toBe(false)
    })
  })

  describe('ゲームの初期化処理', () => {
    it('ゲームの初期状態が正しく設定される', () => {
      expect(game.getGameState()).toBe('ready')
    })

    it('ゲームフィールドが初期化されている', () => {
      expect(game.getField()).toBeDefined()
      expect(game.getField().length).toBe(13) // 高さ13
      expect(game.getField()[0].length).toBe(6) // 幅6
    })

    it('次のぷよが準備されている', () => {
      expect(game.getNextPuyo()).toBeDefined()
    })
  })

  describe('ゲーム画面の表示', () => {
    it('ゲームフィールドが描画される', () => {
      game.renderField()

      // fillRectが呼ばれることを確認（フィールドの背景描画）
      expect(mockContext.fillRect).toHaveBeenCalled()
    })

    it('ゲームフィールドの枠線が描画される', () => {
      game.renderField()

      // strokeRectが呼ばれることを確認（フィールドの枠線描画）
      expect(mockContext.strokeRect).toHaveBeenCalled()
    })

    it('Canvasがクリアされる', () => {
      game.clearScreen()

      // clearRectが呼ばれることを確認
      expect(mockContext.clearRect).toHaveBeenCalled()
    })
  })

  describe('ぷよの生成', () => {
    it('新しいぷよペアが生成される', () => {
      const puyoPair = game.generateNewPuyoPair()

      expect(puyoPair).toBeDefined()
      expect(puyoPair.color1).toBeGreaterThanOrEqual(1)
      expect(puyoPair.color1).toBeLessThanOrEqual(4)
      expect(puyoPair.color2).toBeGreaterThanOrEqual(1)
      expect(puyoPair.color2).toBeLessThanOrEqual(4)
    })

    it('操作ぷよが初期位置に配置される', () => {
      game.spawnActivePuyo()
      const activePuyo = game.getActivePuyo()

      expect(activePuyo).toBeDefined()
      expect(activePuyo!.x).toBe(2) // フィールド中央
      expect(activePuyo!.y).toBe(0) // 上端
    })

    it('次のぷよが更新される', () => {
      const oldNext = game.getNextPuyo()
      game.spawnActivePuyo()
      const newNext = game.getNextPuyo()

      // 次のぷよが新しく生成される
      expect(newNext).not.toEqual(oldNext)
    })
  })
})
