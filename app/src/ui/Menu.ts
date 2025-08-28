import Phaser from 'phaser'
import { Button } from './Button'

/**
 * メニューアイテムの定義
 */
export interface MenuItem {
  label: string
  action: () => void
}

/**
 * ゲーム内メニューコンポーネント
 * 複数のボタンを縦配置で表示するメニューシステム
 */
export class Menu {
  private scene: Phaser.Scene
  private x: number
  private y: number
  private background!: Phaser.GameObjects.Rectangle
  private title!: Phaser.GameObjects.Text
  private buttons: Button[] = []
  private menuItems: MenuItem[] = []

  private readonly BUTTON_WIDTH = 200
  private readonly BUTTON_HEIGHT = 50
  private readonly BUTTON_SPACING = 60
  private readonly MENU_PADDING = 40

  constructor(
    scene: Phaser.Scene,
    x: number,
    y: number,
    menuItems: MenuItem[],
    title: string = 'メニュー'
  ) {
    this.scene = scene
    this.x = x
    this.y = y
    this.menuItems = [...menuItems]

    this.createBackground()
    this.createTitle(title)
    this.createButtons()
  }

  /**
   * メニューの背景を作成
   */
  private createBackground(): void {
    const menuHeight = this.calculateMenuHeight()
    const menuWidth = this.BUTTON_WIDTH + this.MENU_PADDING * 2

    this.background = this.scene.add.rectangle(this.x, this.y, menuWidth, menuHeight, 0x2c3e50)
    this.background.setOrigin(0.5, 0.5)
    this.background.setAlpha(0.9)
    this.background.setStrokeStyle(2, 0x34495e)
  }

  /**
   * メニューのタイトルを作成
   */
  private createTitle(title: string): void {
    this.title = this.scene.add.text(this.x, this.y - 50, title, {
      fontSize: '24px',
      color: '#FFFFFF',
      fontFamily: 'Arial',
    })
    this.title.setOrigin(0.5, 0.5)
  }

  /**
   * メニューボタンを作成
   */
  private createButtons(): void {
    this.menuItems.forEach((item, index) => {
      const buttonY = this.y + index * this.BUTTON_SPACING + 20

      const button = new Button(
        this.scene,
        this.x,
        buttonY,
        this.BUTTON_WIDTH,
        this.BUTTON_HEIGHT,
        item.label,
        item.action
      )

      this.buttons.push(button)
    })
  }

  /**
   * メニューの高さを計算
   */
  private calculateMenuHeight(): number {
    const titleHeight = 50
    const buttonsHeight = this.menuItems.length * this.BUTTON_SPACING + 20
    return titleHeight + buttonsHeight + this.MENU_PADDING
  }

  /**
   * メニューアイテムを追加
   */
  addMenuItem(label: string, action: () => void): void {
    const newItem: MenuItem = { label, action }
    this.menuItems.push(newItem)

    // 新しいボタンを作成
    const buttonY = this.y + (this.menuItems.length - 1) * this.BUTTON_SPACING + 20
    const button = new Button(
      this.scene,
      this.x,
      buttonY,
      this.BUTTON_WIDTH,
      this.BUTTON_HEIGHT,
      label,
      action
    )

    this.buttons.push(button)

    // 背景サイズを再計算
    this.updateBackgroundSize()
  }

  /**
   * メニューアイテムを削除
   */
  removeMenuItem(index: number): void {
    if (index >= 0 && index < this.menuItems.length) {
      // ボタンを破棄
      this.buttons[index].destroy()

      // 配列から削除
      this.menuItems.splice(index, 1)
      this.buttons.splice(index, 1)

      // 残りのボタンの位置を再調整
      this.repositionButtons()

      // 背景サイズを再計算
      this.updateBackgroundSize()
    }
  }

  /**
   * ボタンの位置を再調整
   */
  private repositionButtons(): void {
    this.buttons.forEach((button, index) => {
      const buttonY = this.y + index * this.BUTTON_SPACING + 20
      button.setPosition(this.x, buttonY)
    })
  }

  /**
   * 背景サイズを更新
   */
  private updateBackgroundSize(): void {
    const menuHeight = this.calculateMenuHeight()
    const menuWidth = this.BUTTON_WIDTH + this.MENU_PADDING * 2

    this.background.destroy()
    this.background = this.scene.add.rectangle(this.x, this.y, menuWidth, menuHeight, 0x2c3e50)
    this.background.setOrigin(0.5, 0.5)
    this.background.setAlpha(0.9)
    this.background.setStrokeStyle(2, 0x34495e)
  }

  /**
   * メニューの表示/非表示を設定
   */
  setVisible(visible: boolean): void {
    this.background.setVisible(visible)
    this.title.setVisible(visible)

    this.buttons.forEach((button) => {
      button.setVisible(visible)
    })
  }

  /**
   * メニューを破棄
   */
  destroy(): void {
    this.background.destroy()
    this.title.destroy()

    this.buttons.forEach((button) => {
      button.destroy()
    })

    this.buttons = []
    this.menuItems = []
  }

  /**
   * メニューの位置を設定
   */
  setPosition(x: number, y: number): void {
    this.x = x
    this.y = y

    this.background.setPosition(x, y)
    this.title.setPosition(x, y - 50)

    this.repositionButtons()
  }
}
