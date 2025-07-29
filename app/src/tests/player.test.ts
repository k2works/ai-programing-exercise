import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest'
import { Player } from '../player'

describe('Player', () => {
  let player: Player

  beforeEach(() => {
    player = new Player()
  })

  afterEach(() => {
    vi.restoreAllMocks()
  })

  describe('constructor', () => {
    it('プレイヤーが初期化される', () => {
      expect(player).toBeDefined()
    })
  })

  describe('キーボード入力処理', () => {
    it('下キーが押された状態を管理できる', () => {
      expect(player.isDownKeyPressed()).toBe(false)

      // 下キーが押された
      player.handleKeyDown(40) // ArrowDown
      expect(player.isDownKeyPressed()).toBe(true)

      // 下キーが離された
      player.handleKeyUp(40)
      expect(player.isDownKeyPressed()).toBe(false)
    })

    it('左キーの入力を検出できる', () => {
      const callback = vi.fn()
      player.onLeftKeyPress(callback)

      player.handleKeyDown(37) // ArrowLeft
      expect(callback).toHaveBeenCalled()
    })

    it('右キーの入力を検出できる', () => {
      const callback = vi.fn()
      player.onRightKeyPress(callback)

      player.handleKeyDown(39) // ArrowRight
      expect(callback).toHaveBeenCalled()
    })

    it('上キーの入力を検出できる', () => {
      const callback = vi.fn()
      player.onUpKeyPress(callback)

      player.handleKeyDown(38) // ArrowUp
      expect(callback).toHaveBeenCalled()
    })

    it('無効なキーコードは無視される', () => {
      const callback = vi.fn()
      player.onLeftKeyPress(callback)

      player.handleKeyDown(65) // 'A' key
      expect(callback).not.toHaveBeenCalled()
    })
  })

  describe('操作タイミング管理', () => {
    it('回転操作のタイミングを管理する', () => {
      expect(player.canRotate()).toBe(true)

      player.handleRotation()
      expect(player.canRotate()).toBe(false)

      // タイマーをリセット
      player.resetRotationTimer()
      expect(player.canRotate()).toBe(true)
    })

    it('移動操作のタイミングを管理する', () => {
      expect(player.canMove()).toBe(true)

      player.handleMovement()
      expect(player.canMove()).toBe(false)

      // タイマーをリセット
      player.resetMovementTimer()
      expect(player.canMove()).toBe(true)
    })
  })

  describe('コールバック管理', () => {
    it('複数のコールバックを設定できる', () => {
      const leftCallback = vi.fn()
      const rightCallback = vi.fn()
      const upCallback = vi.fn()

      player.onLeftKeyPress(leftCallback)
      player.onRightKeyPress(rightCallback)
      player.onUpKeyPress(upCallback)

      player.handleKeyDown(37) // ArrowLeft
      expect(leftCallback).toHaveBeenCalled()
      expect(rightCallback).not.toHaveBeenCalled()
      expect(upCallback).not.toHaveBeenCalled()
    })

    it('同じキーに複数のコールバックを設定できる', () => {
      const callback1 = vi.fn()
      const callback2 = vi.fn()

      player.onLeftKeyPress(callback1)
      player.onLeftKeyPress(callback2)

      player.handleKeyDown(37) // ArrowLeft
      expect(callback1).toHaveBeenCalled()
      expect(callback2).toHaveBeenCalled()
    })
  })

  describe('状態リセット', () => {
    it('全ての状態をリセットできる', () => {
      // 各種状態を変更
      player.handleKeyDown(40) // ArrowDown
      player.handleRotation()
      player.handleMovement()

      expect(player.isDownKeyPressed()).toBe(true)
      expect(player.canRotate()).toBe(false)
      expect(player.canMove()).toBe(false)

      // リセット
      player.reset()

      expect(player.isDownKeyPressed()).toBe(false)
      expect(player.canRotate()).toBe(true)
      expect(player.canMove()).toBe(true)
    })
  })

  describe('タイマー更新', () => {
    it('毎フレームタイマーが減少する', () => {
      player.handleRotation()
      player.handleMovement()

      expect(player.canRotate()).toBe(false)
      expect(player.canMove()).toBe(false)

      // 1フレーム更新
      player.updateTimers()

      // まだクールダウン中（初期値から1減っただけ）
      expect(player.canRotate()).toBe(false)
      expect(player.canMove()).toBe(false)
    })

    it('クールダウン時間後に操作可能になる', () => {
      player.handleRotation()
      player.handleMovement()

      // 十分な回数タイマーを更新
      for (let i = 0; i < 20; i++) {
        player.updateTimers()
      }

      expect(player.canRotate()).toBe(true)
      expect(player.canMove()).toBe(true)
    })
  })

  describe('高度な入力処理', () => {
    it('連続した同じキーの入力はクールダウンで制限される', () => {
      const callback = vi.fn()
      player.onLeftKeyPress(callback)

      // 最初の入力
      player.handleKeyDown(37) // ArrowLeft
      expect(callback).toHaveBeenCalledTimes(1)

      // クールダウン中の入力
      player.handleKeyDown(37) // ArrowLeft
      expect(callback).toHaveBeenCalledTimes(1) // 増えない

      // キーリリースをシミュレート
      player.handleKeyUp(37) // ArrowLeft
      
      // タイマーリセット後
      player.resetMovementTimer()
      player.handleKeyDown(37) // ArrowLeft
      expect(callback).toHaveBeenCalledTimes(2) // 増える
    })

    it('異なるキーは独立してクールダウン管理される', () => {
      const leftCallback = vi.fn()
      const upCallback = vi.fn()

      player.onLeftKeyPress(leftCallback)
      player.onUpKeyPress(upCallback)

      // 左キーを押す
      player.handleKeyDown(37) // ArrowLeft
      expect(leftCallback).toHaveBeenCalledTimes(1)

      // 上キーは独立して動作する
      player.handleKeyDown(38) // ArrowUp
      expect(upCallback).toHaveBeenCalledTimes(1)

      // 左キーはまだクールダウン中
      player.handleKeyDown(37) // ArrowLeft
      expect(leftCallback).toHaveBeenCalledTimes(1)
    })
  })

  describe('キーイベントリスナー統合', () => {
    it('keydownイベントリスナーを設定できる', () => {
      const mockElement = {
        addEventListener: vi.fn(),
        removeEventListener: vi.fn(),
      }

      player.setupKeyListeners(mockElement as any)

      expect(mockElement.addEventListener).toHaveBeenCalledWith('keydown', expect.any(Function))
      expect(mockElement.addEventListener).toHaveBeenCalledWith('keyup', expect.any(Function))
    })

    it('キーイベントリスナーを削除できる', () => {
      const mockElement = {
        addEventListener: vi.fn(),
        removeEventListener: vi.fn(),
      }

      player.setupKeyListeners(mockElement as any)
      player.removeKeyListeners(mockElement as any)

      expect(mockElement.removeEventListener).toHaveBeenCalledWith('keydown', expect.any(Function))
      expect(mockElement.removeEventListener).toHaveBeenCalledWith('keyup', expect.any(Function))
    })

    it('実際のキーイベントオブジェクトを処理できる', () => {
      const callback = vi.fn()
      player.onLeftKeyPress(callback)

      // KeyboardEventをシミュレート
      const keyEvent = {
        keyCode: 37,
        preventDefault: vi.fn(),
      } as any

      player.handleKeyboardEvent(keyEvent)

      expect(keyEvent.preventDefault).toHaveBeenCalled()
      expect(callback).toHaveBeenCalled()
    })
  })
})
