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
      // 操作ぷよを着地位置に配置（フィールドの底に配置して重力の影響を受けないようにする）
      const activePuyo = game.getActivePuyo()
      const originalColor1 = activePuyo!.color1
      const originalColor2 = activePuyo!.color2
      activePuyo!.x = 3
      activePuyo!.y = 11 // 底の1つ上に配置（direction=0で縦配置なので底まで到達する）
      activePuyo!.direction = 0

      // 統合処理を実行
      game.processLanding()

      // フィールドに着地したぷよが配置されている（重力処理により底に配置される）
      const field = game.getField()
      expect(field[11][3]).toBe(originalColor1) // 中心ぷよは底の1つ上
      expect(field[12][3]).toBe(originalColor2) // 2つ目のぷよは底

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

  describe('ぷよの回転機能', () => {
    beforeEach(() => {
      // requestAnimationFrame をモック
      vi.stubGlobal('requestAnimationFrame', vi.fn())

      const mockContext = {
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

      const mockCanvas = {
        getContext: vi.fn().mockReturnValue(mockContext),
        width: 320,
        height: 480,
      } as unknown as HTMLCanvasElement

      const mockScoreDisplay = {
        textContent: '',
      } as unknown as HTMLElement

      vi.clearAllMocks()
      game = new Game(mockCanvas, mockScoreDisplay)
      game.spawnActivePuyo()
    })

    describe('回転方向の管理', () => {
      it('アクティブぷよの回転方向を取得できる', () => {
        const direction = game.getActivePuyoDirection()
        expect(direction).toBe(0) // 初期状態は0（縦配置）
      })

      it('上キーで時計回りに回転する', () => {
        const initialDirection = game.getActivePuyoDirection()

        // 上キーを押してupKeyPressedをtrueにする
        const upKeyEvent = new KeyboardEvent('keydown', { key: 'ArrowUp' })
        game.handleKeyDown(upKeyEvent)

        // 回転間隔分だけアップデートを実行して回転処理を行う
        for (let i = 0; i < 15; i++) {
          game.updateAndRender()
        }

        const newDirection = game.getActivePuyoDirection()
        expect(newDirection).toBe((initialDirection + 1) % 4)
      })

      it('回転方向の値は0-3の範囲で循環する', () => {
        // 4回回転させると元の方向に戻る
        for (let i = 0; i < 4; i++) {
          const upKeyEvent = new KeyboardEvent('keydown', { key: 'ArrowUp' })
          game.handleKeyDown(upKeyEvent)

          // 回転間隔分だけアップデートを実行
          for (let j = 0; j < 15; j++) {
            game.updateAndRender()
          }

          // キーアップイベントも送信してキー状態をリセット
          const upKeyUpEvent = new KeyboardEvent('keyup', { key: 'ArrowUp' })
          game.handleKeyUp(upKeyUpEvent)
        }

        expect(game.getActivePuyoDirection()).toBe(0)
      })
    })

    describe('回転後のぷよの位置', () => {
      it('縦配置（方向0）から横配置（方向1）に回転する', () => {
        const activePuyo = game.getActivePuyo()
        expect(activePuyo).not.toBeNull()

        if (activePuyo) {
          const initialX = activePuyo.x
          const initialY = activePuyo.y

          // 上キーで回転
          const upKeyEvent = new KeyboardEvent('keydown', { key: 'ArrowUp' })
          game.handleKeyDown(upKeyEvent)

          // 回転間隔分だけアップデートを実行
          for (let i = 0; i < 15; i++) {
            game.updateAndRender()
          }

          const rotatedPositions = game.getActivePuyoPositions()
          expect(rotatedPositions).toEqual([
            { x: initialX, y: initialY }, // 中心ぷよの位置（変わらず）
            { x: initialX + 1, y: initialY }, // 2つ目のぷよが右に移動
          ])
        }
      })

      it('各回転方向でのぷよの位置が正しい', () => {
        // 回転をテストするため、適切な位置に移動（中央、フィールドの少し下）
        const activePuyo = game.getActivePuyo()
        expect(activePuyo).not.toBeNull()

        // 落下を無効にして純粋に回転のテストを行う
        game.disableFalling()

        if (activePuyo) {
          // より安全な位置(2, 2)に移動してからテスト
          activePuyo.x = 2
          activePuyo.y = 2

          const centerX = activePuyo.x
          const centerY = activePuyo.y

          // 方向0（縦配置、下向き）: 初期状態
          expect(game.getActivePuyoPositions()).toEqual([
            { x: centerX, y: centerY },
            { x: centerX, y: centerY + 1 },
          ])

          // 方向1（横配置、右向き）
          const upKeyEvent1 = new KeyboardEvent('keydown', { key: 'ArrowUp' })
          game.handleKeyDown(upKeyEvent1)
          for (let i = 0; i < 15; i++) {
            game.updateAndRender()
          }
          game.handleKeyUp(new KeyboardEvent('keyup', { key: 'ArrowUp' }))

          expect(game.getActivePuyoPositions()).toEqual([
            { x: centerX, y: centerY },
            { x: centerX + 1, y: centerY },
          ])

          // 方向2（縦配置、上向き）
          const upKeyEvent2 = new KeyboardEvent('keydown', { key: 'ArrowUp' })
          game.handleKeyDown(upKeyEvent2)
          for (let i = 0; i < 15; i++) {
            game.updateAndRender()
          }
          game.handleKeyUp(new KeyboardEvent('keyup', { key: 'ArrowUp' }))

          expect(game.getActivePuyoPositions()).toEqual([
            { x: centerX, y: centerY },
            { x: centerX, y: centerY - 1 },
          ])

          // 方向3（横配置、左向き）
          const upKeyEvent3 = new KeyboardEvent('keydown', { key: 'ArrowUp' })
          game.handleKeyDown(upKeyEvent3)
          for (let i = 0; i < 15; i++) {
            game.updateAndRender()
          }
          game.handleKeyUp(new KeyboardEvent('keyup', { key: 'ArrowUp' }))

          expect(game.getActivePuyoPositions()).toEqual([
            { x: centerX, y: centerY },
            { x: centerX - 1, y: centerY },
          ])
        }
      })
    })

    describe('上キー入力の処理', () => {
      it('上キーが押されたことを検出できる', () => {
        expect(game.isUpKeyPressed()).toBe(false)

        const upKeyEvent = new KeyboardEvent('keydown', { key: 'ArrowUp' })
        game.handleKeyDown(upKeyEvent)

        expect(game.isUpKeyPressed()).toBe(true)
      })

      it('上キーが離されたことを検出できる', () => {
        // まず上キーを押す
        const upKeyDownEvent = new KeyboardEvent('keydown', { key: 'ArrowUp' })
        game.handleKeyDown(upKeyDownEvent)
        expect(game.isUpKeyPressed()).toBe(true)

        // 上キーを離す
        const upKeyUpEvent = new KeyboardEvent('keyup', { key: 'ArrowUp' })
        game.handleKeyUp(upKeyUpEvent)

        expect(game.isUpKeyPressed()).toBe(false)
      })
    })

    describe('回転可能性チェック', () => {
      beforeEach(() => {
        // 落下を無効にして純粋に回転のテストを行う
        game.disableFalling()
        // 安全な位置に操作ぷよを配置
        const activePuyo = game.getActivePuyo()
        if (activePuyo) {
          activePuyo.x = 2
          activePuyo.y = 2
        }
      })

      it('回転可能性をチェックするメソッドが存在する', () => {
        expect(typeof game.canRotate).toBe('function')
      })

      it('フィールド内の空きスペースでは回転可能', () => {
        expect(game.canRotate()).toBe(true)
      })

      it('右端では方向によって回転制限がある', () => {
        const activePuyo = game.getActivePuyo()
        if (activePuyo) {
          activePuyo.x = 5 // 右端
          activePuyo.y = 2
          activePuyo.direction = 0 // 縦配置（下向き）から横配置（右向き）への回転は不可
          expect(game.canRotate()).toBe(false) // 2つ目のぷよがx=6になりフィールド外
        }
      })

      it('左端でも通常は回転可能', () => {
        const activePuyo = game.getActivePuyo()
        if (activePuyo) {
          activePuyo.x = 0 // 左端
          activePuyo.y = 2
          expect(game.canRotate()).toBe(true)
        }
      })

      it('他のぷよがある位置には回転できない', () => {
        const activePuyo = game.getActivePuyo()
        if (activePuyo) {
          // 現在方向0（縦配置、下向き）から方向1（横配置、右向き）への回転を想定
          // 方向1では2つ目のぷよが(x+1, y)の位置に来るので、そこに障害物を置く
          const field = game.getField()
          field[2][3] = 1 // (3, 2)の位置にぷよを配置

          activePuyo.x = 2
          activePuyo.y = 2
          expect(game.canRotate()).toBe(false)
        }
      })

      it('回転後の位置がフィールド外になる場合は回転できない', () => {
        const activePuyo = game.getActivePuyo()
        if (activePuyo) {
          // 方向1（横配置、右向き）から方向2（縦配置、上向き）への回転で、2つ目のぷよがy-1になる場合
          activePuyo.direction = 1 // 横配置（右向き）
          activePuyo.x = 2
          activePuyo.y = 0 // y=0で方向2に回転すると2つ目のぷよがy=-1になる
          expect(game.canRotate()).toBe(false)
        }
      })

      it('上キーが押されても回転できない場合は回転しない', () => {
        const activePuyo = game.getActivePuyo()
        if (activePuyo) {
          // 全方向に障害物を設置して壁キックでも回転できない状況を作る
          const field = game.getField()
          field[2][3] = 1 // 回転先の位置に障害物
          field[2][1] = 1 // 左側（キック先）にも障害物
          field[2][3] = 1 // 右側にも障害物
          field[1][2] = 1 // 上側にも障害物
          field[3][2] = 1 // 下側にも障害物

          activePuyo.x = 2
          activePuyo.y = 2
          const initialDirection = activePuyo.direction

          // 上キーを押して回転を試行
          const upKeyEvent = new KeyboardEvent('keydown', { key: 'ArrowUp' })
          game.handleKeyDown(upKeyEvent)

          // 回転間隔分だけアップデートを実行
          for (let i = 0; i < 15; i++) {
            game.updateAndRender()
          }

          // 回転していないことを確認
          expect(game.getActivePuyoDirection()).toBe(initialDirection)
        }
      })
    })

    describe('壁キック処理', () => {
      beforeEach(() => {
        // 落下を無効にして純粋に回転のテストを行う
        game.disableFalling()
      })

      it('右端で回転時に左にキックして回転できる', () => {
        const activePuyo = game.getActivePuyo()
        if (activePuyo) {
          activePuyo.x = 5 // 右端
          activePuyo.y = 2
          activePuyo.direction = 0 // 縦配置（下向き）

          // 通常では回転できない位置
          expect(game.canRotate()).toBe(false)

          // 上キーを押して回転を試行（壁キックで成功するはず）
          const upKeyEvent = new KeyboardEvent('keydown', { key: 'ArrowUp' })
          game.handleKeyDown(upKeyEvent)

          // 回転間隔分だけアップデートを実行
          for (let i = 0; i < 15; i++) {
            game.updateAndRender()
          }

          // 壁キックにより回転が成功し、左に移動している
          expect(game.getActivePuyoDirection()).toBe(1) // 回転成功
          expect(game.getActivePuyo()!.x).toBe(4) // 左にキックされている
        }
      })

      it('左端で回転時に右にキックして回転できる', () => {
        const activePuyo = game.getActivePuyo()
        if (activePuyo) {
          activePuyo.x = 0 // 左端
          activePuyo.y = 2
          activePuyo.direction = 2 // 縦配置（上向き）から横配置（左向き）への回転

          // 上キーを押して回転を試行（壁キックで成功するはず）
          const upKeyEvent = new KeyboardEvent('keydown', { key: 'ArrowUp' })
          game.handleKeyDown(upKeyEvent)

          // 回転間隔分だけアップデートを実行
          for (let i = 0; i < 15; i++) {
            game.updateAndRender()
          }

          // 壁キックにより回転が成功し、右に移動している
          expect(game.getActivePuyoDirection()).toBe(3) // 回転成功
          expect(game.getActivePuyo()!.x).toBe(1) // 右にキックされている
        }
      })

      it('上端で回転時に下にキックして回転できる', () => {
        const activePuyo = game.getActivePuyo()
        if (activePuyo) {
          activePuyo.x = 2
          activePuyo.y = 0 // 上端
          activePuyo.direction = 1 // 横配置（右向き）から縦配置（上向き）への回転

          // 上キーを押して回転を試行（壁キックで成功するはず）
          const upKeyEvent = new KeyboardEvent('keydown', { key: 'ArrowUp' })
          game.handleKeyDown(upKeyEvent)

          // 回転間隔分だけアップデートを実行
          for (let i = 0; i < 15; i++) {
            game.updateAndRender()
          }

          // 壁キックにより回転が成功し、下に移動している
          expect(game.getActivePuyoDirection()).toBe(2) // 回転成功
          expect(game.getActivePuyo()!.y).toBe(1) // 下にキックされている
        }
      })

      it('壁キックでも回転できない場合は回転しない', () => {
        const activePuyo = game.getActivePuyo()
        if (activePuyo) {
          activePuyo.x = 5 // 右端
          activePuyo.y = 2
          activePuyo.direction = 0 // 縦配置（下向き）

          // 壁キック先にも障害物を設置
          const field = game.getField()
          field[2][4] = 1 // 左側（キック先）にも障害物

          const initialDirection = activePuyo.direction
          const initialX = activePuyo.x

          // 上キーを押して回転を試行
          const upKeyEvent = new KeyboardEvent('keydown', { key: 'ArrowUp' })
          game.handleKeyDown(upKeyEvent)

          // 回転間隔分だけアップデートを実行
          for (let i = 0; i < 15; i++) {
            game.updateAndRender()
          }

          // 壁キックでも回転できないので変化なし
          expect(game.getActivePuyoDirection()).toBe(initialDirection)
          expect(game.getActivePuyo()!.x).toBe(initialX)
        }
      })

      it('壁キック処理をチェックするメソッドが存在する', () => {
        expect(typeof game.tryWallKick).toBe('function')
      })
    })

    describe('下キー入力の処理', () => {
      it('下キーが押されたことを検出できる', () => {
        const downKeyEvent = new KeyboardEvent('keydown', { key: 'ArrowDown' })
        game.handleKeyDown(downKeyEvent)

        expect(game.isDownKeyPressed()).toBe(true)
      })

      it('下キーが離されたことを検出できる', () => {
        // 最初に下キーを押す
        const downKeyEvent = new KeyboardEvent('keydown', { key: 'ArrowDown' })
        game.handleKeyDown(downKeyEvent)
        expect(game.isDownKeyPressed()).toBe(true)

        // 下キーを離す
        const upKeyEvent = new KeyboardEvent('keyup', { key: 'ArrowDown' })
        game.handleKeyUp(upKeyEvent)
        expect(game.isDownKeyPressed()).toBe(false)
      })

      it('入力状態リセットで下キーもリセットされる', () => {
        // 下キーを押す
        const downKeyEvent = new KeyboardEvent('keydown', { key: 'ArrowDown' })
        game.handleKeyDown(downKeyEvent)
        expect(game.isDownKeyPressed()).toBe(true)

        // 入力状態をリセット
        game.resetInputState()
        expect(game.isDownKeyPressed()).toBe(false)
      })
    })

    describe('上向きぷよの着地判定', () => {
      it('上向きぷよが底面まで正しく落下する', () => {
        const activePuyo = game.getActivePuyo()
        if (activePuyo) {
          // 上向きの配置にセット（direction = 2）
          activePuyo.x = 2
          activePuyo.y = 10 // 底面近くから開始
          activePuyo.direction = 2 // 上向き

          // 落下を有効にする
          game.setFallInterval(1) // 毎フレーム落下

          let finalY = -1
          // 底面まで落下させる
          for (let i = 0; i < 10; i++) {
            const currentPuyo = game.getActivePuyo()
            if (currentPuyo) {
              finalY = currentPuyo.y
            }

            game.updateAndRender()

            // 着地したかチェック
            if (!game.getActivePuyo()) {
              break
            }
          }

          // 上向きぷよの最終着地位置をチェック
          // フィールドサイズは13（0-12）で、上向きぷよは2つ目が上（y-1）にある
          // 実際の着地位置は、フィールドの境界に制限される

          // フィールドに固定されているか確認
          const field = game.getField()

          // 着地したぷよがフィールドに配置されている
          let foundPuyos = 0
          for (let y = 0; y < 13; y++) {
            if (field[y][2] !== 0) {
              foundPuyos++
            }
          }
          expect(foundPuyos).toBe(2) // 2つのぷよが配置されている

          // 底面近くにぷよが配置されていることを確認
          expect(field[12][2]).not.toBe(0) // 底面にぷよ
          expect(field[11][2]).not.toBe(0) // その上にぷよ
        }
      })

      it('上向きぷよが他のぷよの上に正しく着地する', () => {
        const activePuyo = game.getActivePuyo()
        if (activePuyo) {
          // フィールドの底部に障害物を設置
          const field = game.getField()
          field[11][2] = 1 // 底部に障害物

          // 上向きの配置にセット
          activePuyo.x = 2
          activePuyo.y = 0
          activePuyo.direction = 2 // 上向き

          // 落下を有効にする
          game.setFallInterval(1) // 毎フレーム落下

          // 障害物に当たるまで落下させる
          for (let i = 0; i < 20; i++) {
            game.updateAndRender()

            // 着地したかチェック
            if (!game.getActivePuyo()) {
              break
            }
          }

          // 新しいぷよが生成されていることを確認（着地完了）
          expect(game.getActivePuyo()).not.toBeNull()

          // フィールドに正しい位置に固定されているか確認
          expect(field[10][2]).not.toBe(0) // 中心ぷよが障害物の上に固定
          expect(field[9][2]).not.toBe(0) // 2つ目のぷよがさらに上に固定
        }
      })
    })

    describe('高速落下処理', () => {
      it('下キーが押されているときは落下速度が上がる', () => {
        const activePuyo = game.getActivePuyo()
        if (activePuyo) {
          // 初期位置を記録
          const initialY = activePuyo.y

          // 通常の落下速度を確認（fallIntervalをデフォルトの30に設定）
          game.setFallInterval(30)

          // 下キーを押す
          const downKeyEvent = new KeyboardEvent('keydown', { key: 'ArrowDown' })
          game.handleKeyDown(downKeyEvent)

          // 1フレーム更新
          game.updateAndRender()

          // 下キーが押されているときは即座に落下するはず
          expect(activePuyo.y).toBeGreaterThan(initialY)
        }
      })

      it('下キーが押されていないときは通常の落下速度', () => {
        const activePuyo = game.getActivePuyo()
        if (activePuyo) {
          // 初期位置を記録
          const initialY = activePuyo.y

          // 通常の落下間隔を設定
          game.setFallInterval(30)

          // 下キーを押さずに1フレーム更新
          game.updateAndRender()

          // 通常の落下間隔なので、まだ落ちないはず
          expect(activePuyo.y).toBe(initialY)
        }
      })

      it('下キーを押し続けると継続的に高速落下する', () => {
        const activePuyo = game.getActivePuyo()
        if (activePuyo) {
          // 初期位置を記録
          const initialY = activePuyo.y

          // 下キーを押す
          const downKeyEvent = new KeyboardEvent('keydown', { key: 'ArrowDown' })
          game.handleKeyDown(downKeyEvent)

          // 複数フレーム更新
          for (let i = 0; i < 5; i++) {
            game.updateAndRender()
          }

          // 継続的に落下しているはず
          expect(activePuyo.y).toBeGreaterThan(initialY + 3)
        }
      })
    })

    describe('落下可能性チェック', () => {
      it('下に空きスペースがあるときは落下可能', () => {
        const activePuyo = game.getActivePuyo()
        if (activePuyo) {
          // 中央上部に配置
          activePuyo.x = 2
          activePuyo.y = 5
          activePuyo.direction = 0 // 縦配置

          // 落下可能かチェック
          expect(game.canFallTest()).toBe(true)
        }
      })

      it('下に障害物があるときは落下不可能', () => {
        const activePuyo = game.getActivePuyo()
        if (activePuyo) {
          // 障害物を設置（y=10とy=11にぷよを配置）
          const field = game.getField()
          field[10][2] = 1 // 障害物
          field[11][2] = 2 // 障害物

          // 障害物の直上に配置
          activePuyo.x = 2
          activePuyo.y = 9
          activePuyo.direction = 0 // 縦配置

          // 落下不可能のはず
          expect(game.canFallTest()).toBe(false)
        }
      })

      it('底面に達したときは落下不可能', () => {
        const activePuyo = game.getActivePuyo()
        if (activePuyo) {
          // 底面に配置
          activePuyo.x = 2
          activePuyo.y = 12 // 底面
          activePuyo.direction = 0 // 縦配置

          // 落下不可能のはず
          expect(game.canFallTest()).toBe(false)
        }
      })

      it('上向きぷよでも正しく落下可能性を判定する', () => {
        const activePuyo = game.getActivePuyo()
        if (activePuyo) {
          // 上向き配置で中央に配置
          activePuyo.x = 2
          activePuyo.y = 5
          activePuyo.direction = 2 // 上向き

          // 落下可能のはず
          expect(game.canFallTest()).toBe(true)
        }
      })

      it('横向きぷよでも正しく落下可能性を判定する', () => {
        const activePuyo = game.getActivePuyo()
        if (activePuyo) {
          // 横向き配置で中央に配置
          activePuyo.x = 2
          activePuyo.y = 5
          activePuyo.direction = 1 // 右向き

          // 落下可能のはず
          expect(game.canFallTest()).toBe(true)
        }
      })
    })

    describe('着地判定', () => {
      it('ぷよが着地したことを正しく検知する', () => {
        const activePuyo = game.getActivePuyo()
        if (activePuyo) {
          // 底面近くに配置
          activePuyo.x = 2
          activePuyo.y = 11 // 底面の1つ上
          activePuyo.direction = 0 // 縦配置

          // 着地判定をテスト
          expect(game.hasLandedTest()).toBe(true)
        }
      })

      it('まだ落下できる状態では着地していない', () => {
        const activePuyo = game.getActivePuyo()
        if (activePuyo) {
          // 中央に配置
          activePuyo.x = 2
          activePuyo.y = 5
          activePuyo.direction = 0 // 縦配置

          // まだ着地していないはず
          expect(game.hasLandedTest()).toBe(false)
        }
      })

      it('他のぷよの上に着地したことを検知する', () => {
        const activePuyo = game.getActivePuyo()
        if (activePuyo) {
          // 障害物を設置
          const field = game.getField()
          field[10][2] = 1 // 障害物

          // 障害物の直上に配置
          activePuyo.x = 2
          activePuyo.y = 9
          activePuyo.direction = 0 // 縦配置（2つ目がy=10になる）

          // 着地しているはず
          expect(game.hasLandedTest()).toBe(true)
        }
      })

      it('複雑な配置でも正しく着地判定する', () => {
        const activePuyo = game.getActivePuyo()
        if (activePuyo) {
          // 複雑な障害物パターンを作成
          const field = game.getField()
          field[11][1] = 1 // 左に障害物
          field[11][3] = 2 // 右に障害物
          field[10][2] = 3 // 中央下に障害物

          // 障害物の上に配置
          activePuyo.x = 2
          activePuyo.y = 9
          activePuyo.direction = 0 // 縦配置

          // 着地しているはず
          expect(game.hasLandedTest()).toBe(true)

          // 落下処理は内部的に処理されるので、直接processLandingを実行
          game.processLanding()

          // 着地により新しいぷよが生成されるはず
          const newActivePuyo = game.getActivePuyo()
          expect(newActivePuyo).not.toBeNull()
          expect(newActivePuyo!.y).toBe(0) // 新しいぷよは初期位置
        }
      })
    })

    describe('統合テスト: 落下から着地まで', () => {
      it('高速落下中も正しく着地判定が働く', () => {
        const activePuyo = game.getActivePuyo()
        if (activePuyo) {
          // 底面近くに配置
          activePuyo.x = 2
          activePuyo.y = 11 // より底面に近い位置
          activePuyo.direction = 0 // 縦配置

          // 下キーを押して高速落下
          const downKeyEvent = new KeyboardEvent('keydown', { key: 'ArrowDown' })
          game.handleKeyDown(downKeyEvent)

          // 高速落下処理を実行（着地するまで）
          game.updateAndRender()

          // 着地により新しいぷよが生成されているはず
          const newActivePuyo = game.getActivePuyo()
          expect(newActivePuyo).not.toBeNull()
          expect(newActivePuyo!.y).toBe(0) // 新しいぷよは初期位置

          // フィールドに前のぷよが固定されているはず
          const field = game.getField()
          expect(field[11][2]).not.toBe(0) // 中心ぷよが固定
          expect(field[12][2]).not.toBe(0) // 2つ目のぷよが固定
        }
      })

      it('回転後も正しく落下可能性を判定する', () => {
        const activePuyo = game.getActivePuyo()
        if (activePuyo) {
          // 底面に配置
          activePuyo.x = 2
          activePuyo.y = 12 // 底面
          activePuyo.direction = 0 // 縦配置

          // 回転して横配置にする
          activePuyo.direction = 1 // 右向き

          // 横配置でも落下可能性を正しく判定するはず
          expect(game.canFallTest()).toBe(false) // 底面なので落下不可
          expect(game.hasLandedTest()).toBe(true) // 着地している
        }
      })
    })
  })

  describe('ぷよの消去処理', () => {
    let game: Game
    let mockCanvas: HTMLCanvasElement
    let mockScoreDisplay: HTMLElement

    beforeEach(() => {
      // Canvas and 2D context mocks
      const mockContext: CanvasRenderingContext2D = {
        fillStyle: '',
        strokeStyle: '',
        lineWidth: 0,
        fillRect: vi.fn(),
        strokeRect: vi.fn(),
        clearRect: vi.fn(),
        beginPath: vi.fn(),
        ellipse: vi.fn(),
        fill: vi.fn(),
        stroke: vi.fn(),
      } as any

      mockCanvas = {
        getContext: vi.fn().mockReturnValue(mockContext),
        width: 300,
        height: 400,
      } as any

      mockScoreDisplay = {
        textContent: '',
      } as any

      // テスト前に新しいゲームインスタンスを作成
      game = new Game(mockCanvas, mockScoreDisplay)
      game.start()
    })

    describe('ぷよの接続判定', () => {
      it('隣接する同じ色のぷよを検出できる', () => {
        // 操作ぷよをクリアしてからテスト用のフィールドを設定
        game.clearActivePuyo()

        // フィールドに同じ色のぷよを隣接して配置
        const field = game.getField()
        field[12][2] = 1 // 赤
        field[12][3] = 1 // 赤（右隣）
        field[11][2] = 1 // 赤（上）

        // 接続判定機能をテスト（startX, startYの順番）
        const connectedPuyos = game.findConnectedPuyos(2, 12)
        expect(connectedPuyos).toHaveLength(3)
        expect(connectedPuyos).toContainEqual({ x: 2, y: 12 })
        expect(connectedPuyos).toContainEqual({ x: 3, y: 12 })
        expect(connectedPuyos).toContainEqual({ x: 2, y: 11 })
      })

      it('異なる色のぷよは接続判定に含まれない', () => {
        // 操作ぷよをクリアしてからテスト用のフィールドを設定
        game.clearActivePuyo()

        // フィールドに異なる色のぷよを配置
        const field = game.getField()
        field[12][2] = 1 // 赤
        field[12][3] = 2 // 緑（異なる色）
        field[11][2] = 1 // 赤

        // 接続判定機能をテスト（startX, startYの順番）
        const connectedPuyos = game.findConnectedPuyos(2, 12)
        expect(connectedPuyos).toHaveLength(2)
        expect(connectedPuyos).toContainEqual({ x: 2, y: 12 })
        expect(connectedPuyos).toContainEqual({ x: 2, y: 11 })
        expect(connectedPuyos).not.toContainEqual({ x: 3, y: 12 })
      })

      it('フィールドの境界外は接続判定に含まれない', () => {
        // 操作ぷよをクリアしてからテスト用のフィールドを設定
        game.clearActivePuyo()

        // フィールドの端にぷよを配置
        const field = game.getField()
        field[12][0] = 1 // 左端
        field[12][1] = 1 // その右

        // 接続判定機能をテスト（左端から開始、startX, startYの順番）
        const connectedPuyos = game.findConnectedPuyos(0, 12)
        expect(connectedPuyos).toHaveLength(2)
        expect(connectedPuyos).toContainEqual({ x: 0, y: 12 })
        expect(connectedPuyos).toContainEqual({ x: 1, y: 12 })
      })

      it('空のセルは接続判定に含まれない', () => {
        // 操作ぷよをクリアしてからテスト用のフィールドを設定
        game.clearActivePuyo()

        // フィールドに孤立したぷよを配置
        const field = game.getField()
        field[12][2] = 1 // 赤（孤立）

        // 接続判定機能をテスト（startX, startYの順番）
        const connectedPuyos = game.findConnectedPuyos(2, 12)
        expect(connectedPuyos).toHaveLength(1)
        expect(connectedPuyos).toContainEqual({ x: 2, y: 12 })
      })
    })

    describe('4つ以上つながったぷよの検出', () => {
      it('4つ以上つながったぷよのグループを検出できる', () => {
        // 操作ぷよをクリアしてからテスト用のフィールドを設定
        game.clearActivePuyo()

        // フィールドにL字型に4つの同じ色のぷよを配置
        const field = game.getField()
        field[12][2] = 1 // 赤
        field[11][2] = 1 // 赤（上）
        field[10][2] = 1 // 赤（上）
        field[10][3] = 1 // 赤（右）

        // 4つ以上つながったぷよのグループを検出
        const eliminateGroups = game.findEliminateGroups()
        expect(eliminateGroups).toHaveLength(1)
        expect(eliminateGroups[0]).toHaveLength(4)
        expect(eliminateGroups[0]).toContainEqual({ x: 2, y: 12 })
        expect(eliminateGroups[0]).toContainEqual({ x: 2, y: 11 })
        expect(eliminateGroups[0]).toContainEqual({ x: 2, y: 10 })
        expect(eliminateGroups[0]).toContainEqual({ x: 3, y: 10 })
      })

      it('3つ以下のぷよは消去対象にならない', () => {
        // 操作ぷよをクリアしてからテスト用のフィールドを設定
        game.clearActivePuyo()

        // フィールドに3つの同じ色のぷよを配置
        const field = game.getField()
        field[12][2] = 1 // 赤
        field[11][2] = 1 // 赤（上）
        field[10][2] = 1 // 赤（上）

        // 3つでは消去対象にならない
        const eliminateGroups = game.findEliminateGroups()
        expect(eliminateGroups).toHaveLength(0)
      })

      it('複数の4つ以上のグループを検出できる', () => {
        // 操作ぷよをクリアしてからテスト用のフィールドを設定
        game.clearActivePuyo()

        // フィールドに2つの独立したグループを配置
        const field = game.getField()
        // 1つ目のグループ（赤）
        field[12][0] = 1
        field[11][0] = 1
        field[10][0] = 1
        field[9][0] = 1

        // 2つ目のグループ（緑）
        field[12][4] = 2
        field[12][5] = 2
        field[11][4] = 2
        field[11][5] = 2

        // 2つのグループを検出
        const eliminateGroups = game.findEliminateGroups()
        expect(eliminateGroups).toHaveLength(2)
        expect(eliminateGroups[0]).toHaveLength(4)
        expect(eliminateGroups[1]).toHaveLength(4)
      })

      it('異なる色が混在している場合は正しく分離される', () => {
        // 操作ぷよをクリアしてからテスト用のフィールドを設定
        game.clearActivePuyo()

        // フィールドに異なる色が混在する配置
        const field = game.getField()
        field[12][2] = 1 // 赤
        field[11][2] = 2 // 緑（異なる色）
        field[10][2] = 1 // 赤
        field[9][2] = 1 // 赤（合計3つなので消去対象外）

        // 連続していない3つの赤は消去対象にならない
        const eliminateGroups = game.findEliminateGroups()
        expect(eliminateGroups).toHaveLength(0)
      })
    })

    describe('ぷよの消去処理', () => {
      it('4つ以上つながったぷよを消去できる', () => {
        // 操作ぷよをクリアしてからテスト用のフィールドを設定
        game.clearActivePuyo()

        // フィールドに4つのつながったぷよを配置
        const field = game.getField()
        field[12][2] = 1 // 赤
        field[11][2] = 1 // 赤（上）
        field[10][2] = 1 // 赤（上）
        field[9][2] = 1 // 赤（上）

        // 消去前の状態を確認
        expect(field[12][2]).toBe(1)
        expect(field[11][2]).toBe(1)
        expect(field[10][2]).toBe(1)
        expect(field[9][2]).toBe(1)

        // 消去処理を実行
        const eliminatedGroups = game.eliminatePuyos()
        expect(eliminatedGroups).toHaveLength(1)
        expect(eliminatedGroups[0]).toHaveLength(4)

        // 消去後の状態を確認
        expect(field[12][2]).toBe(0)
        expect(field[11][2]).toBe(0)
        expect(field[10][2]).toBe(0)
        expect(field[9][2]).toBe(0)
      })

      it('3つ以下のぷよは消去されない', () => {
        // 操作ぷよをクリアしてからテスト用のフィールドを設定
        game.clearActivePuyo()

        // フィールドに3つのつながったぷよを配置
        const field = game.getField()
        field[12][2] = 1 // 赤
        field[11][2] = 1 // 赤（上）
        field[10][2] = 1 // 赤（上）

        // 消去処理を実行
        const eliminatedGroups = game.eliminatePuyos()
        expect(eliminatedGroups).toHaveLength(0)

        // ぷよが残っていることを確認
        expect(field[12][2]).toBe(1)
        expect(field[11][2]).toBe(1)
        expect(field[10][2]).toBe(1)
      })

      it('複数のグループを同時に消去できる', () => {
        // 操作ぷよをクリアしてからテスト用のフィールドを設定
        game.clearActivePuyo()

        // フィールドに2つの独立したグループを配置
        const field = game.getField()
        // 1つ目のグループ（赤）
        field[12][0] = 1
        field[11][0] = 1
        field[10][0] = 1
        field[9][0] = 1

        // 2つ目のグループ（緑）
        field[12][4] = 2
        field[12][5] = 2
        field[11][4] = 2
        field[11][5] = 2

        // 他の色のぷよ（消去されないはず）
        field[8][2] = 3 // 青（孤立）

        // 消去処理を実行
        const eliminatedGroups = game.eliminatePuyos()
        expect(eliminatedGroups).toHaveLength(2)

        // 1つ目のグループが消去されていることを確認
        expect(field[12][0]).toBe(0)
        expect(field[11][0]).toBe(0)
        expect(field[10][0]).toBe(0)
        expect(field[9][0]).toBe(0)

        // 2つ目のグループが消去されていることを確認
        expect(field[12][4]).toBe(0)
        expect(field[12][5]).toBe(0)
        expect(field[11][4]).toBe(0)
        expect(field[11][5]).toBe(0)

        // 他の色のぷよは残っていることを確認
        expect(field[8][2]).toBe(3)
      })

      it('消去対象がない場合は何も消去されない', () => {
        // 操作ぷよをクリアしてからテスト用のフィールドを設定
        game.clearActivePuyo()

        // フィールドに孤立したぷよを配置
        const field = game.getField()
        field[12][2] = 1 // 赤（孤立）
        field[10][4] = 2 // 緑（孤立）

        // 消去処理を実行
        const eliminatedGroups = game.eliminatePuyos()
        expect(eliminatedGroups).toHaveLength(0)

        // ぷよが残っていることを確認
        expect(field[12][2]).toBe(1)
        expect(field[10][4]).toBe(2)
      })
    })

    describe('消去後の落下処理', () => {
      it('消去後に上のぷよが重力で落下する', () => {
        // 操作ぷよをクリアしてからテスト用のフィールドを設定
        game.clearActivePuyo()

        // フィールドに落下テスト用の配置を作成
        const field = game.getField()
        // 下部に消去対象のぷよ（4つ）
        field[12][2] = 1 // 赤
        field[11][2] = 1 // 赤
        field[10][2] = 1 // 赤
        field[9][2] = 1 // 赤

        // 上部に落下すべきぷよ
        field[8][2] = 2 // 緑（落下すべき）
        field[7][2] = 3 // 青（落下すべき）

        // 消去処理を実行
        game.eliminatePuyos()

        // 消去後の落下処理を実行
        game.dropAfterElimination()

        // 上のぷよが落下していることを確認
        expect(field[12][2]).toBe(2) // 緑が一番下に
        expect(field[11][2]).toBe(3) // 青がその上に
        expect(field[10][2]).toBe(0) // 空
        expect(field[9][2]).toBe(0) // 空
        expect(field[8][2]).toBe(0) // 空
        expect(field[7][2]).toBe(0) // 空
      })

      it('複数列で同時に落下処理される', () => {
        // 操作ぷよをクリアしてからテスト用のフィールドを設定
        game.clearActivePuyo()

        // フィールドに複数列の落下テスト用の配置を作成
        const field = game.getField()

        // 1列目: 下部に消去対象
        field[12][1] = 1 // 赤（消去対象）
        field[11][1] = 1 // 赤（消去対象）
        field[10][1] = 1 // 赤（消去対象）
        field[9][1] = 1 // 赤（消去対象）
        field[8][1] = 2 // 緑（落下すべき）

        // 3列目: 下部に消去対象
        field[12][3] = 1 // 赤（消去対象）
        field[11][3] = 1 // 赤（消去対象）
        field[10][3] = 1 // 赤（消去対象）
        field[9][3] = 1 // 赤（消去対象）
        field[8][3] = 3 // 青（落下すべき）
        field[7][3] = 4 // 黄（落下すべき）

        // 消去処理を実行
        game.eliminatePuyos()

        // 消去後の落下処理を実行
        game.dropAfterElimination()

        // 1列目の落下確認
        expect(field[12][1]).toBe(2) // 緑が一番下に
        expect(field[11][1]).toBe(0) // 空
        expect(field[8][1]).toBe(0) // 空

        // 3列目の落下確認
        expect(field[12][3]).toBe(3) // 青が一番下に
        expect(field[11][3]).toBe(4) // 黄がその上に
        expect(field[8][3]).toBe(0) // 空
        expect(field[7][3]).toBe(0) // 空
      })

      it('部分的な消去でも正しく落下する', () => {
        // 操作ぷよをクリアしてからテスト用のフィールドを設定
        game.clearActivePuyo()

        // フィールドに部分消去テスト用の配置を作成
        const field = game.getField()
        field[12][2] = 2 // 緑（残る）
        field[11][2] = 1 // 赤（消去対象）
        field[10][2] = 1 // 赤（消去対象）
        field[9][2] = 1 // 赤（消去対象）
        field[8][2] = 1 // 赤（消去対象）
        field[7][2] = 3 // 青（落下すべき）
        field[6][2] = 4 // 黄（落下すべき）

        // 消去処理を実行
        game.eliminatePuyos()

        // 消去後の落下処理を実行
        game.dropAfterElimination()

        // 落下結果を確認
        expect(field[12][2]).toBe(2) // 緑（元の位置）
        expect(field[11][2]).toBe(3) // 青が落下
        expect(field[10][2]).toBe(4) // 黄が落下
        expect(field[9][2]).toBe(0) // 空
        expect(field[8][2]).toBe(0) // 空
        expect(field[7][2]).toBe(0) // 空
        expect(field[6][2]).toBe(0) // 空
      })

      it('消去対象がない場合は落下処理も何も起こらない', () => {
        // 操作ぷよをクリアしてからテスト用のフィールドを設定
        game.clearActivePuyo()

        // フィールドに消去対象がない配置を作成（隙間なく下に詰めた状態）
        const field = game.getField()
        field[12][2] = 1 // 赤（最下段）
        field[11][2] = 2 // 緑（その上）
        field[10][2] = 3 // 青（その上）

        // 元の状態を記録
        const originalField = field.map((row) => [...row])

        // 消去処理を実行
        const eliminatedGroups = game.eliminatePuyos()

        // 消去対象がないことを確認
        expect(eliminatedGroups).toHaveLength(0)

        // 消去後の落下処理を実行
        game.dropAfterElimination()

        // フィールドが変わっていないことを確認
        expect(field).toEqual(originalField)
      })
    })
  })

  describe('連鎖反応システム', () => {
    describe('連鎖判定', () => {
      it('消去後の落下で新たな4つ以上のつながりができた場合は連鎖発生を検出する', () => {
        // 操作ぷよをクリアしてからテスト用のフィールドを設定
        game.clearActivePuyo()

        // 連鎖が発生する配置を作成
        const field = game.getField()

        // 最初の消去対象：赤のぷよが4つ縦に並ぶ
        field[12][1] = 1 // 赤
        field[11][1] = 1 // 赤
        field[10][1] = 1 // 赤
        field[9][1] = 1 // 赤

        // 落下後に連鎖となるぷよ配置
        field[8][1] = 2 // 緑（落下後に連鎖対象になる）
        field[12][2] = 2 // 緑（連鎖対象）
        field[11][2] = 2 // 緑（連鎖対象）
        field[10][2] = 2 // 緑（連鎖対象）

        // 連鎖処理を実行
        const chainResult = game.processChain()

        // 連鎖が検出されることを確認
        expect(chainResult.chains).toBeGreaterThan(0)
        expect(chainResult.totalEliminated).toBeGreaterThan(4) // 最初の4個 + 連鎖で消去された数
      })

      it('消去後に新たなつながりができない場合は連鎖が発生しない', () => {
        // 操作ぷよをクリアしてからテスト用のフィールドを設定
        game.clearActivePuyo()

        // 連鎖が発生しない配置を作成
        const field = game.getField()

        // 消去対象：赤のぷよが4つ
        field[12][1] = 1 // 赤
        field[11][1] = 1 // 赤
        field[10][1] = 1 // 赤
        field[9][1] = 1 // 赤

        // 孤立したぷよ（連鎖にならない）
        field[8][2] = 2 // 緑（孤立）
        field[6][3] = 3 // 青（孤立）

        // 連鎖処理を実行
        const chainResult = game.processChain()

        // 連鎖が発生しないことを確認
        expect(chainResult.chains).toBe(1) // 初回の消去のみ
        expect(chainResult.totalEliminated).toBe(4) // 最初の4個のみ
      })

      it('多段連鎖が正しく処理される', () => {
        // 操作ぷよをクリアしてからテスト用のフィールドを設定
        game.clearActivePuyo()

        // 3連鎖が発生する複雑な配置を作成
        const field = game.getField()

        // 2連鎖パターン: L字型配置
        // 底の支えとなるぷよ
        field[12][1] = 4 // 黄（支え用）
        field[12][2] = 4 // 黄（支え用）

        // 1連鎖目：赤のぷよ（横4つ）
        field[11][0] = 1 // 赤
        field[11][1] = 1 // 赤
        field[11][2] = 1 // 赤
        field[11][3] = 1 // 赤

        // 2連鎖目準備：緑のぷよ（赤が消えた後に落下してつながる）
        field[10][0] = 2 // 緑（落下後に赤の場所へ）
        field[9][0] = 2 // 緑（落下後に赤の場所へ）
        field[8][0] = 2 // 緑（落下後に赤の場所へ）
        field[12][0] = 2 // 緑（元から底にある）

        // 連鎖処理を実行
        const chainResult = game.processChain()

        // 2連鎖が発生することを確認
        expect(chainResult.chains).toBe(2)
        expect(chainResult.totalEliminated).toBe(8) // 4×2連鎖 = 8個
      })
    })

    describe('連鎖カウントとスコア計算', () => {
      it('基本スコア計算が正しく動作する', () => {
        // 操作ぷよをクリアしてからテスト用のフィールドを設定
        game.clearActivePuyo()

        const field = game.getField()

        // 単純な4つつながりでスコア計算をテスト
        field[12][0] = 1 // 赤
        field[11][0] = 1 // 赤
        field[10][0] = 1 // 赤
        field[9][0] = 1 // 赤

        // スコア計算を実行
        const score = game.calculateScore(1, 4, 1) // 1連鎖, 4個消去, 1色

        // 基本スコア: 4個 × 10 × (連鎖ボーナス8 + 個数ボーナス2 + 色ボーナス0) = 400
        expect(score).toBe(400)
      })

      it('連鎖ボーナスが正しく計算される', () => {
        // 操作ぷよをクリアしてからテスト用のフィールドを設定
        game.clearActivePuyo()

        // 2連鎖のスコア計算をテスト
        const score2 = game.calculateScore(2, 4, 1) // 2連鎖, 4個消去, 1色
        expect(score2).toBe(720) // 4 × 10 × (16 + 2 + 0) = 720

        // 3連鎖のスコア計算をテスト
        const score3 = game.calculateScore(3, 4, 1) // 3連鎖, 4個消去, 1色
        expect(score3).toBe(1360) // 4 × 10 × (32 + 2 + 0) = 1360
      })

      it('色ボーナスが正しく計算される', () => {
        // 操作ぷよをクリアしてからテスト用のフィールドを設定
        game.clearActivePuyo()

        // 2色同時消去のスコア計算をテスト
        const score2colors = game.calculateScore(1, 8, 2) // 1連鎖, 8個消去, 2色
        expect(score2colors).toBe(1440) // 8 × 10 × (8 + 6 + 4) = 1440

        // 3色同時消去のスコア計算をテスト
        const score3colors = game.calculateScore(1, 12, 3) // 1連鎖, 12個消去, 3色
        expect(score3colors).toBe(2640) // 12 × 10 × (8 + 6 + 8) = 2640
      })

      it('連鎖処理と統合したスコア計算が動作する', () => {
        // 操作ぷよをクリアしてからテスト用のフィールドを設定
        game.clearActivePuyo()

        const field = game.getField()

        // 2連鎖パターンを作成
        field[12][1] = 4 // 黄（支え用）
        field[12][2] = 4 // 黄（支え用）

        // 1連鎖目：赤のぷよ（横4つ）
        field[11][0] = 1 // 赤
        field[11][1] = 1 // 赤
        field[11][2] = 1 // 赤
        field[11][3] = 1 // 赤

        // 2連鎖目準備：緑のぷよ
        field[10][0] = 2 // 緑
        field[9][0] = 2 // 緑
        field[8][0] = 2 // 緑
        field[12][0] = 2 // 緑

        // 初期スコアを記録
        const initialScore = game.getScore()

        // 連鎖処理とスコア計算を実行
        const chainResult = game.processChainWithScore()

        // 2連鎖が発生し、スコアが加算されることを確認
        expect(chainResult.chains).toBe(2)
        expect(chainResult.totalScore).toBeGreaterThan(0)
        expect(game.getScore()).toBeGreaterThan(initialScore)
      })
    })

    describe('スコア表示システム', () => {
      it('初期スコアが正しく表示される', () => {
        // 初期スコアは0
        expect(game.getScore()).toBe(0)

        // スコア表示要素の更新を確認
        expect(scoreDisplay.textContent).toBe('スコア: 0')
      })

      it('スコアが更新されたときに表示も更新される', () => {
        // 操作ぷよをクリアしてからテスト用のフィールドを設定
        game.clearActivePuyo()

        // 4つの赤ぷよを配置して消去
        const field = game.getField()
        field[12][0] = field[12][1] = field[12][2] = field[12][3] = 1

        // 連鎖処理とスコア計算を実行
        const result = game.processChainWithScore()

        // スコアが更新されていることを確認
        expect(game.getScore()).toBe(result.totalScore)
        expect(game.getScore()).toBeGreaterThan(0)

        // スコア表示要素も更新されていることを確認
        expect(scoreDisplay.textContent).toBe(`スコア: ${result.totalScore}`)
      })

      it('複数回の連鎖でスコアが累積される', () => {
        // 操作ぷよをクリアしてからテスト用のフィールドを設定
        game.clearActivePuyo()

        // 最初の連鎖を実行
        const field = game.getField()
        field[12][0] = field[12][1] = field[12][2] = field[12][3] = 1
        const result1 = game.processChainWithScore()
        const firstScore = result1.totalScore

        // フィールドをクリアして次の連鎖用に設定
        for (let y = 0; y < 13; y++) {
          for (let x = 0; x < 6; x++) {
            field[y][x] = 0
          }
        }
        field[12][0] = field[12][1] = field[12][2] = field[12][3] = 2

        // 2回目の連鎖を実行
        const result2 = game.processChainWithScore()
        const secondScore = result2.totalScore

        // スコアが累積されていることを確認
        expect(game.getScore()).toBe(firstScore + secondScore)
        expect(scoreDisplay.textContent).toBe(`スコア: ${firstScore + secondScore}`)
      })
    })

    describe('ゲームループ統合', () => {
      it('ぷよが着地したときに自動的に連鎖処理が実行される', () => {
        // 着地時の連鎖処理をテストするためにフィールドを設定
        game.clearActivePuyo()
        const field = game.getField()

        // 4つの赤ぷよを配置（着地したらすぐ消えるように）
        field[12][1] = field[12][2] = field[12][3] = field[12][4] = 1

        // 操作ぷよとして赤ぷよを設定（着地すると5つになって消去される）
        game.spawnActivePuyo()
        const activePuyo = game.getActivePuyo()
        if (activePuyo) {
          activePuyo.x = 0
          activePuyo.y = 11
          activePuyo.color1 = 1 // 赤
          activePuyo.color2 = 2 // 緑（消去には関係ない）
        }

        // 初期スコアを記録
        const initialScore = game.getScore()

        // 着地処理を実行（この時点で連鎖処理も自動実行されるべき）
        game.processLanding()

        // 連鎖が発生してスコアが上がっていることを確認
        expect(game.getScore()).toBeGreaterThan(initialScore)

        // 消去されたぷよがフィールドから消えていることを確認
        let redCount = 0
        for (let y = 0; y < 13; y++) {
          for (let x = 0; x < 6; x++) {
            if (field[y][x] === 1) redCount++
          }
        }
        expect(redCount).toBeLessThan(5) // 5つあった赤ぷよが消去されているはず
      })

      it('連鎖が発生しない場合は通常通り次のぷよを生成する', () => {
        // 連鎖が発生しないパターンをテスト
        game.clearActivePuyo()
        const field = game.getField()

        // 単独の赤ぷよを配置（着地しても消去されない）
        field[12][2] = 1

        // 操作ぷよとして青ぷよを設定
        game.spawnActivePuyo()
        const activePuyo = game.getActivePuyo()
        if (activePuyo) {
          activePuyo.x = 1
          activePuyo.y = 11
          activePuyo.color1 = 3 // 青
          activePuyo.color2 = 2 // 緑
        }

        // 初期スコアを記録
        const initialScore = game.getScore()

        // 着地処理を実行
        game.processLanding()

        // スコアは変わらない（連鎖が発生していない）
        expect(game.getScore()).toBe(initialScore)

        // 新しい操作ぷよが生成されていることを確認
        expect(game.getActivePuyo()).not.toBeNull()
      })
    })
  })

  describe('フィールドの重力処理', () => {
    it('消去処理なしでも空間がある場合はぷよが落下する', () => {
      // フィールドに浮いているぷよがある状況を作成
      game.clearActivePuyo()
      const field = game.getField()

      // 浮いているぷよを配置（下に空間がある）
      field[10][2] = 1 // 赤ぷよが浮いている
      field[12][2] = 2 // 緑ぷよが底にある（間にfield[11][2]が空）

      // 手動で重力処理を実行
      const dropped = game.dropAfterElimination()

      // 落下が発生したことを確認
      expect(dropped).toBe(true)

      // 浮いていたぷよが正しい位置に落下していることを確認
      expect(field[10][2]).toBe(0) // 元の位置は空
      expect(field[11][2]).toBe(1) // 赤ぷよが落下してここに配置
      expect(field[12][2]).toBe(2) // 緑ぷよはそのまま
    })

    it('複数のぷよが重なって浮いている場合も正しく落下する', () => {
      game.clearActivePuyo()
      const field = game.getField()

      // 複数のぷよが浮いている状況
      field[8][1] = 1 // 赤
      field[9][1] = 2 // 緑
      field[10][1] = 3 // 青（これらが浮いている）
      field[12][1] = 4 // 黄が底にある（field[11][1]が空）

      // 重力処理を実行
      const dropped = game.dropAfterElimination()

      // 落下が発生したことを確認
      expect(dropped).toBe(true)

      // すべてのぷよが正しい位置に落下していることを確認
      expect(field[8][1]).toBe(0) // 元の位置は空
      expect(field[9][1]).toBe(1) // 赤が一番上に落下
      expect(field[10][1]).toBe(2) // 緑がその下に落下
      expect(field[11][1]).toBe(3) // 青がその下に落下
      expect(field[12][1]).toBe(4) // 黄は元の位置のまま
    })

    it('操作ぷよ着地後に自動的に重力処理が実行される', () => {
      // フィールドに浮いているぷよがある状況を作成
      game.clearActivePuyo()
      const field = game.getField()

      // 浮いているぷよを配置
      field[10][2] = 1 // 赤ぷよが浮いている
      field[12][2] = 2 // 緑ぷよが底にある（間にfield[11][2]が空）

      // 操作ぷよを別の場所に配置して着地させる
      game.spawnActivePuyo()
      const activePuyo = game.getActivePuyo()
      if (activePuyo) {
        activePuyo.x = 0
        activePuyo.y = 11 // 底の1つ上の位置
        activePuyo.direction = 0 // 縦配置
        activePuyo.color1 = 3 // 青
        activePuyo.color2 = 4 // 黄
      }

      // 着地処理を実行（実際のゲームプレイを模擬）
      game.processLanding()

      // 操作ぷよの着地とは関係なく、浮いていたぷよが落下していることを確認
      expect(field[10][2]).toBe(0) // 元の位置は空
      expect(field[11][2]).toBe(1) // 赤ぷよが落下
      expect(field[12][2]).toBe(2) // 緑ぷよはそのまま
    })

    it('複数回の重力処理で段階的に落下する', () => {
      game.clearActivePuyo()
      const field = game.getField()

      // 段階的に落下が必要な状況を作成
      field[8][3] = 1 // 赤
      field[10][3] = 2 // 緑（間にfield[9][3]が空）
      field[12][3] = 3 // 青（間にfield[11][3]が空）

      // 操作ぷよを別の場所で着地させる
      game.spawnActivePuyo()
      const activePuyo = game.getActivePuyo()
      if (activePuyo) {
        activePuyo.x = 0
        activePuyo.y = 11 // フィールド高さ13なので最大12、1つ上の11に配置
        activePuyo.direction = 0 // 縦配置
        activePuyo.color1 = 4
        activePuyo.color2 = 4
      }

      // 着地処理を実行
      game.processLanding()

      // すべてのぷよが正しく落下していることを確認
      expect(field[8][3]).toBe(0) // 元の位置は空
      expect(field[9][3]).toBe(0) // 元の位置は空
      expect(field[10][3]).toBe(1) // 赤が落下
      expect(field[11][3]).toBe(2) // 緑が落下
      expect(field[12][3]).toBe(3) // 青はそのまま
    })
  })
})
