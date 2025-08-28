import { describe, it, expect, beforeEach, vi } from 'vitest'
import Phaser from 'phaser'
import { Menu } from './Menu'
import { Button } from './Button'

// Button クラスをモック
vi.mock('./Button', () => ({
  Button: vi.fn().mockImplementation(() => ({
    setPosition: vi.fn(),
    setVisible: vi.fn(),
    destroy: vi.fn(),
    enabled: true,
  })),
}))

describe('Menu', () => {
  let scene: Phaser.Scene
  let menu: Menu

  beforeEach(() => {
    // Mock scene
    scene = {
      add: {
        rectangle: vi.fn().mockReturnValue({
          setOrigin: vi.fn().mockReturnThis(),
          setAlpha: vi.fn().mockReturnThis(),
          setStrokeStyle: vi.fn().mockReturnThis(),
          setVisible: vi.fn().mockReturnThis(),
          destroy: vi.fn(),
        }),
        text: vi.fn().mockReturnValue({
          setOrigin: vi.fn().mockReturnThis(),
          setStyle: vi.fn().mockReturnThis(),
          setVisible: vi.fn().mockReturnThis(),
          destroy: vi.fn(),
        }),
      },
    } as unknown as Phaser.Scene

    // Button モックをリセット
    vi.clearAllMocks()
  })

  describe('コンストラクタ', () => {
    it('メニューが正しく初期化される', () => {
      const menuItems = [
        { label: 'スタート', action: vi.fn() },
        { label: '設定', action: vi.fn() },
        { label: '終了', action: vi.fn() },
      ]

      menu = new Menu(scene, 400, 300, menuItems)

      expect(scene.add.rectangle).toHaveBeenCalled() // 背景
      expect(scene.add.text).toHaveBeenCalled() // タイトル
      expect(Button).toHaveBeenCalledTimes(3) // 3つのボタン
    })

    it('カスタムタイトルでメニューが初期化される', () => {
      const menuItems = [{ label: 'アイテム1', action: vi.fn() }]

      menu = new Menu(scene, 400, 300, menuItems, 'カスタムメニュー')

      expect(scene.add.text).toHaveBeenCalledWith(400, 250, 'カスタムメニュー', expect.any(Object))
    })
  })

  describe('メニューアイテム管理', () => {
    it('メニューアイテムが正しく配置される', () => {
      const menuItems = [
        { label: 'アイテム1', action: vi.fn() },
        { label: 'アイテム2', action: vi.fn() },
      ]

      menu = new Menu(scene, 400, 300, menuItems)

      // ボタンが適切な位置に配置されることを確認
      expect(Button).toHaveBeenNthCalledWith(
        1,
        scene,
        400,
        320,
        200,
        50,
        'アイテム1',
        expect.any(Function)
      )
      expect(Button).toHaveBeenNthCalledWith(
        2,
        scene,
        400,
        380,
        200,
        50,
        'アイテム2',
        expect.any(Function)
      )
    })

    it('メニューアイテムを追加できる', () => {
      const menuItems = [{ label: '既存アイテム', action: vi.fn() }]

      menu = new Menu(scene, 400, 300, menuItems)

      const newAction = vi.fn()
      menu.addMenuItem('新しいアイテム', newAction)

      // 新しいボタンが作成されることを確認
      expect(Button).toHaveBeenCalledTimes(2)
    })

    it('メニューアイテムを削除できる', () => {
      const menuItems = [
        { label: 'アイテム1', action: vi.fn() },
        { label: 'アイテム2', action: vi.fn() },
      ]

      menu = new Menu(scene, 400, 300, menuItems)

      menu.removeMenuItem(0)

      // ボタンの destroy メソッドが呼ばれることを確認
      const MockedButton = Button as ReturnType<typeof vi.fn>
      const firstButtonInstance = MockedButton.mock.results[0].value
      expect(firstButtonInstance.destroy).toHaveBeenCalled()
    })
  })

  describe('表示制御', () => {
    it('メニューを表示/非表示にできる', () => {
      const menuItems = [{ label: 'テスト', action: vi.fn() }]

      menu = new Menu(scene, 400, 300, menuItems)

      menu.setVisible(false)
      menu.setVisible(true)

      // 背景とタイトルの表示状態が変更されることを確認
      const mockBackground = scene.add.rectangle as ReturnType<typeof vi.fn>
      const mockTitle = scene.add.text as ReturnType<typeof vi.fn>
      const backgroundInstance = mockBackground.mock.results[0].value
      const titleInstance = mockTitle.mock.results[0].value

      expect(backgroundInstance.setVisible).toHaveBeenCalledWith(false)
      expect(backgroundInstance.setVisible).toHaveBeenCalledWith(true)
      expect(titleInstance.setVisible).toHaveBeenCalledWith(false)
      expect(titleInstance.setVisible).toHaveBeenCalledWith(true)
    })
  })

  describe('破棄処理', () => {
    it('メニューが正しく破棄される', () => {
      const menuItems = [
        { label: 'テスト1', action: vi.fn() },
        { label: 'テスト2', action: vi.fn() },
      ]

      menu = new Menu(scene, 400, 300, menuItems)

      menu.destroy()

      // 全ての要素が破棄されることを確認
      const mockBackground = scene.add.rectangle as ReturnType<typeof vi.fn>
      const mockTitle = scene.add.text as ReturnType<typeof vi.fn>
      const backgroundInstance = mockBackground.mock.results[0].value
      const titleInstance = mockTitle.mock.results[0].value

      expect(backgroundInstance.destroy).toHaveBeenCalled()
      expect(titleInstance.destroy).toHaveBeenCalled()

      // 全てのボタンが破棄されることを確認
      const MockedButton = Button as ReturnType<typeof vi.fn>
      MockedButton.mock.results.forEach((result) => {
        expect(result.value.destroy).toHaveBeenCalled()
      })
    })
  })
})
