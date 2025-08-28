// エフェクトシステムのメインエクスポート
import { EffectManager } from './EffectManager'
import { CharacterAnimator } from './CharacterAnimator'
import { BackgroundManager } from './BackgroundManager'

export { EffectManager } from './EffectManager'
export { CharacterAnimator } from './CharacterAnimator'
export { BackgroundManager } from './BackgroundManager'

// 統合エフェクトマネージャー
export class IntegratedEffectManager {
  private effectManager: EffectManager
  private characterAnimator: CharacterAnimator
  private backgroundManager: BackgroundManager

  constructor(container: any, scene: any) {
    this.effectManager = new EffectManager(container)
    this.characterAnimator = new CharacterAnimator(scene)
    this.backgroundManager = new BackgroundManager(scene)
  }

  // EffectManager のメソッドを公開
  get effect() {
    return this.effectManager
  }

  // CharacterAnimator のメソッドを公開
  get character() {
    return this.characterAnimator
  }

  // BackgroundManager のメソッドを公開
  get background() {
    return this.backgroundManager
  }

  /**
   * すべてのエフェクトを停止
   */
  stopAll(): void {
    this.effectManager.stopAllEffects()
    this.characterAnimator.stopAllAnimations()
    this.backgroundManager.stopAllAnimations()
  }

  /**
   * すべてのリソースを破棄
   */
  destroy(): void {
    this.effectManager.destroy()
    this.characterAnimator.destroy()
    this.backgroundManager.destroy()
  }
}
