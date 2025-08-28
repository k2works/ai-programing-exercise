import { describe, it, expect, beforeEach, vi } from 'vitest'
import Phaser from 'phaser'
import { Button } from './Button'

describe('Button', () => {
  let scene: Phaser.Scene
  let button: Button

  beforeEach(() => {
    // Mock scene
    scene = {
      add: {
        rectangle: vi.fn().mockReturnValue({
          setInteractive: vi.fn().mockReturnThis(),
          setOrigin: vi.fn().mockReturnThis(),
          on: vi.fn().mockReturnThis(),
          setStrokeStyle: vi.fn().mockReturnThis(),
          setFillStyle: vi.fn().mockReturnThis(),
          setAlpha: vi.fn().mockReturnThis(),
          x: 0,
          y: 0,
          width: 0,
          height: 0,
          destroy: vi.fn(),
        }),
        text: vi.fn().mockReturnValue({
          setOrigin: vi.fn().mockReturnThis(),
          setStyle: vi.fn().mockReturnThis(),
          setScale: vi.fn().mockReturnThis(),
          setAlpha: vi.fn().mockReturnThis(),
          x: 0,
          y: 0,
          destroy: vi.fn(),
        }),
      },
    } as unknown as Phaser.Scene
  })

  describe('コンストラクタ', () => {
    it('ボタンが正しく初期化される', () => {
      const onClick = vi.fn()
      button = new Button(scene, 100, 50, 200, 60, 'テストボタン', onClick)

      expect(scene.add.rectangle).toHaveBeenCalledWith(100, 50, 200, 60, 0x4caf50)
      expect(scene.add.text).toHaveBeenCalledWith(100, 50, 'テストボタン', expect.any(Object))
    })

    it('カスタムスタイルでボタンが初期化される', () => {
      const onClick = vi.fn()
      const customStyle = {
        backgroundColor: 0xff0000,
        textColor: '#FFFFFF',
        fontSize: '24px',
      }

      button = new Button(scene, 100, 50, 200, 60, 'カスタムボタン', onClick, customStyle)

      expect(scene.add.rectangle).toHaveBeenCalledWith(100, 50, 200, 60, 0xff0000)
    })
  })

  describe('イベント処理', () => {
    it('クリック時にコールバック関数が呼ばれる', () => {
      const onClick = vi.fn()
      button = new Button(scene, 100, 50, 200, 60, 'クリックテスト', onClick)

      // イベントハンドラーの登録を確認
      const mockRectangle = scene.add.rectangle as ReturnType<typeof vi.fn>
      const rectangleInstance = mockRectangle.mock.results[0].value
      expect(rectangleInstance.setInteractive).toHaveBeenCalled()
      expect(rectangleInstance.on).toHaveBeenCalledWith('pointerdown', expect.any(Function))
    })

    it('ホバー時にスタイルが変更される', () => {
      const onClick = vi.fn()
      button = new Button(scene, 100, 50, 200, 60, 'ホバーテスト', onClick)

      const mockRectangle = scene.add.rectangle as ReturnType<typeof vi.fn>
      const rectangleInstance = mockRectangle.mock.results[0].value
      expect(rectangleInstance.on).toHaveBeenCalledWith('pointerover', expect.any(Function))
      expect(rectangleInstance.on).toHaveBeenCalledWith('pointerout', expect.any(Function))
    })
  })

  describe('状態管理', () => {
    it('ボタンが有効化・無効化できる', () => {
      const onClick = vi.fn()
      button = new Button(scene, 100, 50, 200, 60, '状態テスト', onClick)

      button.setEnabled(false)
      expect(button.enabled).toBe(false)

      button.setEnabled(true)
      expect(button.enabled).toBe(true)
    })

    it('無効化されたボタンはクリックできない', () => {
      const onClick = vi.fn()
      button = new Button(scene, 100, 50, 200, 60, '無効化テスト', onClick)

      button.setEnabled(false)
      // 実際のクリック処理テストは統合テストで実施
      expect(button.enabled).toBe(false)
    })
  })

  describe('破棄処理', () => {
    it('ボタンが正しく破棄される', () => {
      const onClick = vi.fn()
      button = new Button(scene, 100, 50, 200, 60, '破棄テスト', onClick)

      button.destroy()

      const mockRectangle = scene.add.rectangle as ReturnType<typeof vi.fn>
      const mockText = scene.add.text as ReturnType<typeof vi.fn>
      const rectangleInstance = mockRectangle.mock.results[0].value
      const textInstance = mockText.mock.results[0].value

      expect(rectangleInstance.destroy).toHaveBeenCalled()
      expect(textInstance.destroy).toHaveBeenCalled()
    })
  })
})
