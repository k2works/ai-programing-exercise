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
      game.spawnActivePuyo()
      const newNext = game.getNextPuyo()

      // 次のぷよが新しく必ず生成される
      expect(newNext).toBeDefined()
      expect(newNext.color1).toBeGreaterThanOrEqual(1)
      expect(newNext.color1).toBeLessThanOrEqual(4)
      expect(newNext.color2).toBeGreaterThanOrEqual(1)
      expect(newNext.color2).toBeLessThanOrEqual(4)
    })
  })

  describe('ぷよの描画', () => {
    beforeEach(() => {
      // fillRectメソッドをリセット
      vi.clearAllMocks()
    })

    it('操作ぷよが画面に描画される', () => {
      game.spawnActivePuyo()
      game.renderActivePuyo()

      // 2つのぷよが描画される（fillRectが2回呼ばれる）
      expect(mockContext.fillRect).toHaveBeenCalledTimes(2)
    })

    it('次のぷよが画面に描画される', () => {
      game.renderNextPuyo()

      // 次のぷよエリアが描画される
      expect(mockContext.fillRect).toHaveBeenCalled()
    })

    it('ぷよの色が正しく設定される', () => {
      game.spawnActivePuyo()
      game.renderActivePuyo()

      // fillStyleが設定される
      expect(mockContext.fillStyle).toBeDefined()
    })
  })

  describe('ぷよの落下', () => {
    beforeEach(() => {
      vi.clearAllMocks()
    })

    it('操作ぷよが自動的に落下する', () => {
      game.spawnActivePuyo()
      const initialPuyo = game.getActivePuyo()
      const initialY = initialPuyo!.y

      // 落下間隔分だけ時間を進めて落下処理を実行
      for (let i = 0; i < 30; i++) {
        game.updateFalling()
      }

      const updatedPuyo = game.getActivePuyo()
      expect(updatedPuyo!.y).toBeGreaterThan(initialY)
    })

    it('操作ぷよが下に障害物があるときは落下しない', () => {
      game.spawnActivePuyo()
      // フィールドの底部に到達した状態をシミュレート
      const activePuyo = game.getActivePuyo()
      activePuyo!.y = 11 // フィールドの下の方

      // 落下間隔分だけ時間を進める
      for (let i = 0; i < 30; i++) {
        game.updateFalling()
      }

      // 底部に到達しているので位置が変わらない
      expect(activePuyo!.y).toBe(11)
    })

    it('落下速度が設定できる', () => {
      const speed = game.getFallSpeed()
      expect(speed).toBeGreaterThan(0)
    })
  })

  describe('プレイヤー入力検出', () => {
    beforeEach(() => {
      vi.clearAllMocks()
      // キーボードイベントのモック設定
      global.addEventListener = vi.fn()
      global.removeEventListener = vi.fn()
    })

    it('左キーが押されたことを検知できる', () => {
      const leftKeyEvent = new KeyboardEvent('keydown', { key: 'ArrowLeft' })
      game.handleKeyDown(leftKeyEvent)

      expect(game.isLeftKeyPressed()).toBe(true)
    })

    it('右キーが押されたことを検知できる', () => {
      const rightKeyEvent = new KeyboardEvent('keydown', { key: 'ArrowRight' })
      game.handleKeyDown(rightKeyEvent)

      expect(game.isRightKeyPressed()).toBe(true)
    })

    it('キーが離されたことを検知できる', () => {
      // 左キーを押した後に離す
      const leftKeyDownEvent = new KeyboardEvent('keydown', { key: 'ArrowLeft' })
      game.handleKeyDown(leftKeyDownEvent)

      const leftKeyUpEvent = new KeyboardEvent('keyup', { key: 'ArrowLeft' })
      game.handleKeyUp(leftKeyUpEvent)

      expect(game.isLeftKeyPressed()).toBe(false)
    })

    it('関係ないキーは無視される', () => {
      const spaceKeyEvent = new KeyboardEvent('keydown', { key: ' ' })
      game.handleKeyDown(spaceKeyEvent)

      expect(game.isLeftKeyPressed()).toBe(false)
      expect(game.isRightKeyPressed()).toBe(false)
    })

    it('キーボードイベントリスナーが登録される', () => {
      game.setupInputHandlers()

      expect(global.addEventListener).toHaveBeenCalledWith('keydown', expect.any(Function))
      expect(global.addEventListener).toHaveBeenCalledWith('keyup', expect.any(Function))
    })

    it('入力状態をリセットできる', () => {
      // 先にキーを押した状態にする
      const leftKeyEvent = new KeyboardEvent('keydown', { key: 'ArrowLeft' })
      game.handleKeyDown(leftKeyEvent)
      expect(game.isLeftKeyPressed()).toBe(true)

      // リセットする
      game.resetInputState()

      expect(game.isLeftKeyPressed()).toBe(false)
      expect(game.isRightKeyPressed()).toBe(false)
    })
  })
})
