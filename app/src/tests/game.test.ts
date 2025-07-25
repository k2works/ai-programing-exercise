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
      lineWidth: 1,
      fillRect: vi.fn(),
      strokeRect: vi.fn(),
      clearRect: vi.fn(),
      drawImage: vi.fn(),
      beginPath: vi.fn(),
      ellipse: vi.fn(),
      fill: vi.fn(),
      stroke: vi.fn(),
    } as unknown as CanvasRenderingContext2D

    // Canvas要素をモック
    canvas = document.createElement('canvas')
    canvas.width = 320
    canvas.height = 480

    // getContextメソッドをモック
    vi.spyOn(canvas, 'getContext').mockReturnValue(mockContext)

    // スコア表示要素をモック
    scoreDisplay = document.createElement('div')

    // requestAnimationFrameをモック
    vi.stubGlobal('requestAnimationFrame', vi.fn())

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

    it('start()を呼ぶとキーボードイベントリスナーが設定される', () => {
      const setupInputHandlersSpy = vi.spyOn(game, 'setupInputHandlers')

      game.start()

      expect(setupInputHandlersSpy).toHaveBeenCalled()
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

      // 2つのぷよが楕円形で描画される（ellipseが2回呼ばれる）
      expect(mockContext.ellipse).toHaveBeenCalledTimes(2)
      expect(mockContext.fill).toHaveBeenCalledTimes(2)
    })

    it('次のぷよが画面に描画される', () => {
      game.renderNextPuyo()

      // 次のぷよエリアが楕円形で描画される
      expect(mockContext.ellipse).toHaveBeenCalled()
      expect(mockContext.fill).toHaveBeenCalled()
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

  describe('ぷよの移動', () => {
    beforeEach(() => {
      vi.clearAllMocks()
      // 操作ぷよを生成しておく
      game.spawnActivePuyo()
    })

    it('左キーが押されたときぷよが左に移動する', () => {
      const initialPuyo = game.getActivePuyo()
      const initialX = initialPuyo!.x

      // 左キーを押す
      const leftKeyEvent = new KeyboardEvent('keydown', { key: 'ArrowLeft' })
      game.handleKeyDown(leftKeyEvent)

      // 移動間隔分だけ時間を進めて移動処理を実行
      for (let i = 0; i < 8; i++) {
        game.updateMovement()
      }

      const updatedPuyo = game.getActivePuyo()
      expect(updatedPuyo!.x).toBe(initialX - 1)
    })

    it('右キーが押されたときぷよが右に移動する', () => {
      const initialPuyo = game.getActivePuyo()
      const initialX = initialPuyo!.x

      // 右キーを押す
      const rightKeyEvent = new KeyboardEvent('keydown', { key: 'ArrowRight' })
      game.handleKeyDown(rightKeyEvent)

      // 移動間隔分だけ時間を進めて移動処理を実行
      for (let i = 0; i < 8; i++) {
        game.updateMovement()
      }

      const updatedPuyo = game.getActivePuyo()
      expect(updatedPuyo!.x).toBe(initialX + 1)
    })

    it('キーが押されていないときは移動しない', () => {
      const initialPuyo = game.getActivePuyo()
      const initialX = initialPuyo!.x

      // キーを押さずに移動処理を実行
      game.updateMovement()

      const updatedPuyo = game.getActivePuyo()
      expect(updatedPuyo!.x).toBe(initialX)
    })

    it('操作ぷよがないときは移動処理をしない', () => {
      // 操作ぷよをクリア
      game.clearActivePuyo()

      // 左キーを押して移動処理を実行
      const leftKeyEvent = new KeyboardEvent('keydown', { key: 'ArrowLeft' })
      game.handleKeyDown(leftKeyEvent)
      game.updateMovement()

      // 操作ぷよがないのでエラーにならない
      expect(game.getActivePuyo()).toBeNull()
    })

    it('左右のキーが同時に押されたときは移動しない', () => {
      const initialPuyo = game.getActivePuyo()
      const initialX = initialPuyo!.x

      // 左右のキーを同時に押す
      const leftKeyEvent = new KeyboardEvent('keydown', { key: 'ArrowLeft' })
      const rightKeyEvent = new KeyboardEvent('keydown', { key: 'ArrowRight' })
      game.handleKeyDown(leftKeyEvent)
      game.handleKeyDown(rightKeyEvent)

      // 移動間隔分だけ時間を進めて移動処理を実行
      for (let i = 0; i < 8; i++) {
        game.updateMovement()
      }

      const updatedPuyo = game.getActivePuyo()
      expect(updatedPuyo!.x).toBe(initialX) // 移動しない
    })

    it('移動間隔までは移動しない', () => {
      const initialPuyo = game.getActivePuyo()
      const initialX = initialPuyo!.x

      // 左キーを押す
      const leftKeyEvent = new KeyboardEvent('keydown', { key: 'ArrowLeft' })
      game.handleKeyDown(leftKeyEvent)

      // 移動間隔の直前まで移動処理を実行
      for (let i = 0; i < 7; i++) {
        game.updateMovement()
      }

      const updatedPuyo = game.getActivePuyo()
      expect(updatedPuyo!.x).toBe(initialX) // まだ移動しない
    })

    it('連続移動のタイミングが正しく制御される', () => {
      const initialPuyo = game.getActivePuyo()
      const initialX = initialPuyo!.x

      // 左キーを押して8フレーム待って移動させる
      const leftKeyEvent = new KeyboardEvent('keydown', { key: 'ArrowLeft' })
      game.handleKeyDown(leftKeyEvent)

      for (let i = 0; i < 8; i++) {
        game.updateMovement()
      }

      // 最初の移動が完了
      expect(game.getActivePuyo()!.x).toBe(initialX - 1)

      // さらに8フレーム待って再び移動
      for (let i = 0; i < 8; i++) {
        game.updateMovement()
      }

      const updatedPuyo = game.getActivePuyo()
      expect(updatedPuyo!.x).toBe(initialX - 2) // 2回移動
    })
  })

  describe('移動後の表示更新', () => {
    beforeEach(() => {
      vi.clearAllMocks()
      game.spawnActivePuyo()
    })

    it('移動後にrenderが呼ばれて表示が更新される', () => {
      // renderメソッドをスパイする
      const renderSpy = vi.spyOn(game, 'render')

      // 左キーを押して移動させる
      const leftKeyEvent = new KeyboardEvent('keydown', { key: 'ArrowLeft' })
      game.handleKeyDown(leftKeyEvent)

      // ゲームループを1回実行
      game.updateAndRender()

      // renderが呼ばれることを確認
      expect(renderSpy).toHaveBeenCalled()
    })

    it('キーが押されていない場合でもrenderは呼ばれる', () => {
      // renderメソッドをスパイする
      const renderSpy = vi.spyOn(game, 'render')

      // キーを押さずにゲームループを実行
      game.updateAndRender()

      // renderが呼ばれることを確認
      expect(renderSpy).toHaveBeenCalled()
    })

    it('移動処理の前後でぷよの描画位置が変わる', () => {
      const initialPuyo = game.getActivePuyo()
      const initialX = initialPuyo!.x

      // renderActivePuyoメソッドをスパイする
      const renderActivePuyoSpy = vi.spyOn(game, 'renderActivePuyo')

      // 左キーを押す
      const leftKeyEvent = new KeyboardEvent('keydown', { key: 'ArrowLeft' })
      game.handleKeyDown(leftKeyEvent)

      // 移動間隔分だけ時間を進めて移動処理を実行
      for (let i = 0; i < 8; i++) {
        game.updateMovement()
      }

      // 表示を更新
      game.render()

      // ぷよの位置が変わっていることを確認
      const updatedPuyo = game.getActivePuyo()
      expect(updatedPuyo!.x).toBe(initialX - 1)

      // renderActivePuyoが呼ばれることを確認
      expect(renderActivePuyoSpy).toHaveBeenCalled()
    })

    it('ゲームループが動作中は継続的に表示が更新される', async () => {
      // requestAnimationFrameをモック
      const mockRAF = vi.fn((callback) => {
        // すぐにコールバックを実行
        setTimeout(callback, 0)
        return 1
      })
      vi.stubGlobal('requestAnimationFrame', mockRAF)

      // renderメソッドをスパイする
      const renderSpy = vi.spyOn(game, 'render')

      // ゲームを開始
      game.start()

      // 少し待ってからstop
      await new Promise((resolve) => setTimeout(resolve, 10))
      game.stop()

      // renderが呼ばれることを確認
      expect(renderSpy).toHaveBeenCalled()
    })
  })

  describe('移動可能性チェック', () => {
    beforeEach(() => {
      vi.clearAllMocks()
      game.spawnActivePuyo()
    })

    it('左端（x=0）でさらに左に移動しようとしても移動できない', () => {
      // 操作ぷよを左端に移動
      const activePuyo = game.getActivePuyo()
      activePuyo!.x = 0

      const leftKeyEvent = new KeyboardEvent('keydown', { key: 'ArrowLeft' })
      game.handleKeyDown(leftKeyEvent)

      // 移動間隔分だけ時間を進める
      for (let i = 0; i < 8; i++) {
        game.updateMovement()
      }

      // 左端にいるので移動できない
      expect(game.getActivePuyo()!.x).toBe(0)
    })

    it('右端（x=5）でさらに右に移動しようとしても移動できない', () => {
      // 操作ぷよを右端に移動
      const activePuyo = game.getActivePuyo()
      activePuyo!.x = 5 // FIELD_WIDTH - 1

      const rightKeyEvent = new KeyboardEvent('keydown', { key: 'ArrowRight' })
      game.handleKeyDown(rightKeyEvent)

      // 移動間隔分だけ時間を進める
      for (let i = 0; i < 8; i++) {
        game.updateMovement()
      }

      // 右端にいるので移動できない
      expect(game.getActivePuyo()!.x).toBe(5)
    })

    it('フィールド内では正常に左移動できる', () => {
      // 操作ぷよを中央付近に配置
      const activePuyo = game.getActivePuyo()
      activePuyo!.x = 3

      const leftKeyEvent = new KeyboardEvent('keydown', { key: 'ArrowLeft' })
      game.handleKeyDown(leftKeyEvent)

      // 移動間隔分だけ時間を進める
      for (let i = 0; i < 8; i++) {
        game.updateMovement()
      }

      // 正常に左に移動できる
      expect(game.getActivePuyo()!.x).toBe(2)
    })

    it('フィールド内では正常に右移動できる', () => {
      // 操作ぷよを中央付近に配置
      const activePuyo = game.getActivePuyo()
      activePuyo!.x = 1

      const rightKeyEvent = new KeyboardEvent('keydown', { key: 'ArrowRight' })
      game.handleKeyDown(rightKeyEvent)

      // 移動間隔分だけ時間を進める
      for (let i = 0; i < 8; i++) {
        game.updateMovement()
      }

      // 正常に右に移動できる
      expect(game.getActivePuyo()!.x).toBe(2)
    })

    it('他のぷよがある位置には移動できない', () => {
      // フィールドにぷよを配置
      const field = game.getField()
      field[5][1] = 1 // (1, 5)の位置にぷよを配置

      // 操作ぷよを隣に配置
      const activePuyo = game.getActivePuyo()
      activePuyo!.x = 2
      activePuyo!.y = 5

      const leftKeyEvent = new KeyboardEvent('keydown', { key: 'ArrowLeft' })
      game.handleKeyDown(leftKeyEvent)

      // 移動間隔分だけ時間を進める
      for (let i = 0; i < 8; i++) {
        game.updateMovement()
      }

      // 他のぷよがあるので移動できない
      expect(game.getActivePuyo()!.x).toBe(2)
    })

    it('移動可能性チェックメソッドが左端でfalseを返す', () => {
      const activePuyo = game.getActivePuyo()
      activePuyo!.x = 0

      expect(game.canMoveLeft()).toBe(false)
    })

    it('移動可能性チェックメソッドが右端でfalseを返す', () => {
      const activePuyo = game.getActivePuyo()
      activePuyo!.x = 5

      expect(game.canMoveRight()).toBe(false)
    })

    it('移動可能性チェックメソッドがフィールド内でtrueを返す', () => {
      const activePuyo = game.getActivePuyo()
      activePuyo!.x = 2

      expect(game.canMoveLeft()).toBe(true)
      expect(game.canMoveRight()).toBe(true)
    })

    it('フィールドの範囲外は移動できない', () => {
      const activePuyo = game.getActivePuyo()

      // 左端のテスト
      activePuyo!.x = 0
      expect(game.canMoveLeft()).toBe(false)

      // 右端のテスト
      activePuyo!.x = 5
      expect(game.canMoveRight()).toBe(false)

      // 中央のテスト
      activePuyo!.x = 2
      expect(game.canMoveLeft()).toBe(true)
      expect(game.canMoveRight()).toBe(true)
    })

    it('ぷよの2つ目が他のぷよと衝突する場合も移動できない', () => {
      // フィールドにぷよを配置（操作ぷよの2つ目の位置に当たる場所）
      const field = game.getField()
      field[6][1] = 1 // (1, 6)の位置にぷよを配置

      // 操作ぷよを配置
      const activePuyo = game.getActivePuyo()
      activePuyo!.x = 2
      activePuyo!.y = 5

      // 左に移動しようとすると、2つ目のぷよが(1,6)に衝突する
      expect(game.canMoveLeft()).toBe(false)
    })
  })

  describe('ぷよの着地検出', () => {
    beforeEach(() => {
      vi.clearAllMocks()
      game.start()
    })

    it('ぷよが底面に着地したことを検出できる', () => {
      // 操作ぷよを底面近くに配置
      const activePuyo = game.getActivePuyo()
      activePuyo!.y = 11 // 2つ目のぷよが底面(y=12)に接触

      expect(game.canMoveDown()).toBe(false)
    })

    it('ぷよが他のぷよに着地したことを検出できる', () => {
      // フィールドにぷよを配置
      const field = game.getField()
      field[8][2] = 1 // (2, 8)の位置にぷよを配置

      // 操作ぷよをその上に配置
      const activePuyo = game.getActivePuyo()
      activePuyo!.x = 2
      activePuyo!.y = 6 // 2つ目のぷよがy=7で、下のぷよ(y=8)に接触

      expect(game.canMoveDown()).toBe(false)
    })

    it('ぷよが落下可能な状態を検出できる', () => {
      // 操作ぷよを中間の位置に配置
      const activePuyo = game.getActivePuyo()
      activePuyo!.x = 2
      activePuyo!.y = 5

      expect(game.canMoveDown()).toBe(true)
    })

    it('ぷよの1つ目が他のぷよに衝突する場合も着地判定する', () => {
      // フィールドにぷよを配置
      const field = game.getField()
      field[6][2] = 1 // (2, 6)の位置にぷよを配置

      // 操作ぷよをそのすぐ上に配置
      const activePuyo = game.getActivePuyo()
      activePuyo!.x = 2
      activePuyo!.y = 5 // 1つ目のぷよがy=5で、下のぷよ(y=6)に接触

      expect(game.canMoveDown()).toBe(false)
    })

    it('着地判定メソッドが存在する', () => {
      expect(typeof game.canMoveDown).toBe('function')
    })
  })

  describe('着地後の次ぷよ生成', () => {
    beforeEach(() => {
      vi.clearAllMocks()
      game.start()
    })

    it('ぷよが着地したら操作ぷよをフィールドに固定する', () => {
      // 操作ぷよを底面近くに配置
      const activePuyo = game.getActivePuyo()
      activePuyo!.x = 2
      activePuyo!.y = 11 // 2つ目のぷよが底面(y=12)に接触

      // 操作ぷよをフィールドに固定する処理
      game.landActivePuyo()

      // フィールドに操作ぷよが固定されている
      const field = game.getField()
      expect(field[11][2]).toBe(activePuyo!.color1) // 1つ目のぷよ
      expect(field[12][2]).toBe(activePuyo!.color2) // 2つ目のぷよ

      // 操作ぷよがクリアされている
      expect(game.getActivePuyo()).toBeNull()
    })

    it('着地後に新しい操作ぷよが生成される', () => {
      // 元の次ぷよの色を記録
      const originalNextPuyo = game.getNextPuyo()

      // 操作ぷよを着地させる
      const activePuyo = game.getActivePuyo()
      activePuyo!.x = 2
      activePuyo!.y = 11
      game.landActivePuyo()

      // 新しい操作ぷよが生成される
      game.spawnActivePuyo()
      const newActivePuyo = game.getActivePuyo()

      expect(newActivePuyo).not.toBeNull()
      expect(newActivePuyo!.x).toBe(2) // 初期位置
      expect(newActivePuyo!.y).toBe(0) // 初期位置
      expect(newActivePuyo!.color1).toBe(originalNextPuyo.color1)
      expect(newActivePuyo!.color2).toBe(originalNextPuyo.color2)
    })

    it('着地処理メソッドが存在する', () => {
      expect(typeof game.landActivePuyo).toBe('function')
    })

    it('着地処理でフィールドの正しい位置にぷよが配置される', () => {
      // 操作ぷよを特定の位置に配置
      const activePuyo = game.getActivePuyo()
      activePuyo!.x = 1
      activePuyo!.y = 9
      activePuyo!.color1 = 1
      activePuyo!.color2 = 2

      // 着地処理を実行
      game.landActivePuyo()

      // フィールドの正しい位置にぷよが配置されている
      const field = game.getField()
      expect(field[9][1]).toBe(1) // 1つ目のぷよ
      expect(field[10][1]).toBe(2) // 2つ目のぷよ
    })

    it('着地処理と次ぷよ生成を統合したメソッドが存在する', () => {
      expect(typeof game.processLanding).toBe('function')
    })

    it('統合メソッドで着地から次ぷよ生成まで一括処理される', () => {
      // 操作ぷよを着地位置に配置
      const activePuyo = game.getActivePuyo()
      const originalColor1 = activePuyo!.color1
      const originalColor2 = activePuyo!.color2
      activePuyo!.x = 3
      activePuyo!.y = 10

      // 統合処理を実行
      game.processLanding()

      // フィールドに着地したぷよが配置されている
      const field = game.getField()
      expect(field[10][3]).toBe(originalColor1)
      expect(field[11][3]).toBe(originalColor2)

      // 新しい操作ぷよが生成されている
      const newActivePuyo = game.getActivePuyo()
      expect(newActivePuyo).not.toBeNull()
      expect(newActivePuyo!.x).toBe(2) // 初期位置
      expect(newActivePuyo!.y).toBe(0) // 初期位置
    })
  })

  describe('着地後の固定処理', () => {
    let game: Game
    let mockCanvas: HTMLCanvasElement
    let mockContext: CanvasRenderingContext2D
    let mockScoreDisplay: HTMLElement

    beforeEach(() => {
      // Canvas とコンテキストのモックを作成
      mockContext = {
        fillStyle: '',
        fillRect: vi.fn(),
        strokeStyle: '',
        strokeRect: vi.fn(),
        clearRect: vi.fn(),
      } as unknown as CanvasRenderingContext2D

      mockCanvas = {
        getContext: vi.fn().mockReturnValue(mockContext),
        width: 320,
        height: 480,
      } as unknown as HTMLCanvasElement

      mockScoreDisplay = {
        textContent: '',
      } as unknown as HTMLElement

      vi.clearAllMocks()
      game = new Game(mockCanvas, mockScoreDisplay)
      game.spawnActivePuyo()
    })

    it('着地したぷよは左右に移動できない', () => {
      // 操作ぷよを底部近くに配置
      const activePuyo = game.getActivePuyo()
      activePuyo!.x = 2
      activePuyo!.y = 11 // フィールド底部 - 1

      // 落下タイマーが満たされるまで処理を実行（着地するはず）
      for (let i = 0; i < 30; i++) {
        game.updateFalling()
      }

      // 着地後は操作ぷよが新しく生成されている
      expect(game.getActivePuyo()).not.toBeNull()
      expect(game.getActivePuyo()!.x).toBe(2) // 初期位置
      expect(game.getActivePuyo()!.y).toBe(0) // 初期位置

      // フィールドに前のぷよが固定されている
      const field = game.getField()
      expect(field[11][2]).not.toBe(0) // 着地したぷよが固定されている
      expect(field[12][2]).not.toBe(0) // 着地したぷよが固定されている
    })

    it('底部に着地したときに自動的に着地処理が実行される', () => {
      // 操作ぷよを底部の一つ上に配置
      const activePuyo = game.getActivePuyo()
      activePuyo!.x = 2
      activePuyo!.y = 11 // フィールド底部 - 1（y=11, 二つ目のぷよがy=12）
      const originalColor1 = activePuyo!.color1
      const originalColor2 = activePuyo!.color2

      // 最初にcanFall()をチェック
      expect(game['canFall']()).toBe(false)

      // 落下処理を実行（fallTimerが満たされるまで繰り返す）
      for (let i = 0; i < 30; i++) {
        game.updateFalling()
      }

      // 操作ぷよが新しく生成されている（着地処理が実行された）
      expect(game.getActivePuyo()).not.toBeNull()
      expect(game.getActivePuyo()!.x).toBe(2) // 初期位置
      expect(game.getActivePuyo()!.y).toBe(0) // 初期位置

      // フィールドにぷよが配置されている
      const field = game.getField()
      expect(field[11][2]).toBe(originalColor1)
      expect(field[12][2]).toBe(originalColor2)
    })
  })

  describe('フィールド上のぷよ描画', () => {
    let game: Game
    let mockCanvas: HTMLCanvasElement
    let mockContext: CanvasRenderingContext2D
    let mockScoreDisplay: HTMLElement

    beforeEach(() => {
      // Canvas とコンテキストのモックを作成
      mockContext = {
        fillStyle: '',
        strokeStyle: '',
        lineWidth: 1,
        fillRect: vi.fn(),
        strokeRect: vi.fn(),
        clearRect: vi.fn(),
        beginPath: vi.fn(),
        ellipse: vi.fn(),
        fill: vi.fn(),
        stroke: vi.fn(),
      } as unknown as CanvasRenderingContext2D

      mockCanvas = {
        getContext: vi.fn().mockReturnValue(mockContext),
        width: 320,
        height: 480,
      } as unknown as HTMLCanvasElement

      mockScoreDisplay = {
        textContent: '',
      } as unknown as HTMLElement

      vi.clearAllMocks()
      game = new Game(mockCanvas, mockScoreDisplay)
      game.spawnActivePuyo()
    })

    it('フィールドに固定されたぷよが描画される', () => {
      // フィールドにぷよを配置
      const field = game.getField()
      field[10][2] = 1 // 赤いぷよ
      field[11][2] = 2 // 緑のぷよ
      field[12][3] = 3 // 青いぷよ

      // フィールドを描画
      game.renderField()

      // ellipseが呼ばれている回数を確認（楕円描画）
      const ellipseCalls = (mockContext.ellipse as any).mock.calls

      // 固定ぷよが楕円形で描画される（3回のellipse呼び出しがあるはず）
      expect(ellipseCalls.length).toBe(3)

      // fillが呼ばれている回数を確認（楕円の塗りつぶし）
      const fillCalls = (mockContext.fill as any).mock.calls
      expect(fillCalls.length).toBe(3)

      // ぷよの色が正しく設定されているかチェック
      const fillStyleCalls = mockContext.fillStyle as any

      // 固定されたぷよの色が描画されているかをテスト
      game.render()
      expect(mockContext.fillRect).toHaveBeenCalled()
    })

    it('空のフィールドセルは描画されない', () => {
      // 空のフィールドで描画
      game.renderField()

      const fillRectCalls = (mockContext.fillRect as any).mock.calls

      // 背景のみの描画（固定ぷよがないので背景の1回のみ）
      expect(fillRectCalls.length).toBe(1)
    })
  })
})
