import { Character, Position } from './Character'
import * as Phaser from 'phaser'

/**
 * キャラクター管理システム
 * 複数のキャラクターの表示・非表示・アセット管理を一元化
 */
export class CharacterManager {
  private scene: Phaser.Scene
  private characters: Map<string, Character> = new Map()
  private characterConfigs: Map<string, string[]> = new Map()

  constructor(scene: Phaser.Scene) {
    this.scene = scene
  }

  /**
   * キャラクターを登録する
   * @param name キャラクター名
   * @param expressions 利用可能な表情リスト
   */
  registerCharacter(name: string, expressions: string[]): void {
    this.characterConfigs.set(name, expressions)
    this.characters.set(name, new Character(this.scene, name))
  }

  /**
   * キャラクターが登録されているかチェック
   * @param name キャラクター名
   * @returns 登録されている場合true
   */
  hasCharacter(name: string): boolean {
    return this.characters.has(name)
  }

  /**
   * キャラクターインスタンスを取得
   * @param name キャラクター名
   * @returns キャラクターインスタンス
   */
  getCharacter(name: string): Character | undefined {
    return this.characters.get(name)
  }

  /**
   * キャラクターを表示する
   * @param name キャラクター名
   * @param expression 表情
   * @param position 表示位置
   */
  showCharacter(name: string, expression: string, position: Position): void {
    const character = this.characters.get(name)
    if (!character) {
      throw new Error(`Character ${name} is not registered`)
    }

    character.show(expression, position)
  }

  /**
   * キャラクターを非表示にする
   * @param name キャラクター名
   */
  hideCharacter(name: string): void {
    const character = this.characters.get(name)
    if (character) {
      character.hide()
    }
  }

  /**
   * すべてのキャラクターを非表示にする
   */
  hideAllCharacters(): void {
    this.characters.forEach((character) => {
      character.hide()
    })
  }

  /**
   * キャラクターの表情を変更する
   * @param name キャラクター名
   * @param expression 新しい表情
   */
  changeCharacterExpression(name: string, expression: string): void {
    const character = this.characters.get(name)
    if (character) {
      character.changeExpression(expression)
    }
  }

  /**
   * キャラクターアセットをプリロードする
   * @param name キャラクター名
   */
  preloadCharacterAssets(name: string): void {
    const expressions = this.characterConfigs.get(name)
    if (!expressions) {
      return
    }

    expressions.forEach((expression) => {
      const assetKey = `${name}-${expression}`
      const assetPath = `assets/characters/${name}/${expression}.png`
      this.scene.load.image(assetKey, assetPath)
    })
  }

  /**
   * 登録されているすべてのキャラクターアセットをプリロードする
   */
  preloadAllCharacterAssets(): void {
    this.characterConfigs.forEach((_, name) => {
      this.preloadCharacterAssets(name)
    })
  }

  /**
   * 指定された位置にキャラクターを移動する
   * @param name キャラクター名
   * @param position 新しい位置
   */
  moveCharacter(name: string, position: Position): void {
    const character = this.characters.get(name)
    if (character) {
      character.setPosition(position)
    }
  }

  /**
   * フェードイン効果でキャラクターを表示する
   * @param name キャラクター名
   * @param expression 表情
   * @param position 表示位置
   * @param duration フェード時間
   */
  showCharacterWithFadeIn(
    name: string,
    expression: string,
    position: Position,
    duration: number = 500
  ): void {
    const character = this.characters.get(name)
    if (!character) {
      throw new Error(`Character ${name} is not registered`)
    }

    character.showWithFadeIn(expression, position, duration)
  }

  /**
   * フェードアウト効果でキャラクターを非表示にする
   * @param name キャラクター名
   * @param duration フェード時間
   */
  hideCharacterWithFadeOut(name: string, duration: number = 500): void {
    const character = this.characters.get(name)
    if (character) {
      character.hideWithFadeOut(duration)
    }
  }

  /**
   * 全キャラクターのクリーンアップ
   */
  destroy(): void {
    this.characters.forEach((character) => {
      character.destroy()
    })
    this.characters.clear()
    this.characterConfigs.clear()
  }
}
