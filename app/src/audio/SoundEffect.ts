import Phaser from 'phaser'

/**
 * AudioManagerのインターフェース（循環参照を避けるため）
 */
interface IAudioManager {
  getMasterVolume(): number
  getSeVolume(): number
  isMuted(): boolean
}

/**
 * 効果音管理クラス
 * 事前定義された効果音と任意の効果音の再生を管理
 */
export class SoundEffect {
  private scene: Phaser.Scene
  private audioManager?: IAudioManager

  /**
   * コンストラクタ
   * @param scene Phaserシーン
   * @param audioManager オーディオマネージャー（音量制御用）
   */
  constructor(scene: Phaser.Scene, audioManager?: IAudioManager) {
    this.scene = scene
    this.audioManager = audioManager
  }

  /**
   * クリック音を再生
   */
  playClick(): void {
    this.play('click')
  }

  /**
   * ホバー音を再生
   */
  playHover(): void {
    this.play('hover')
  }

  /**
   * 確定音を再生
   */
  playConfirm(): void {
    this.play('confirm')
  }

  /**
   * キャンセル音を再生
   */
  playCancel(): void {
    this.play('cancel')
  }

  /**
   * エラー音を再生
   */
  playError(): void {
    this.play('error')
  }

  /**
   * 指定したキーの効果音を再生
   * @param key 音声キー
   * @param volume 音量（指定時はオーディオマネージャーの設定を上書き）
   */
  play(key: string, volume?: number): void {
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    const sound: any = this.scene.sound.add(key)

    if (volume !== undefined) {
      sound.setVolume(volume)
    } else if (this.audioManager) {
      // AudioManagerから音量設定を取得
      const masterVolume = this.audioManager.getMasterVolume()
      const seVolume = this.audioManager.getSeVolume()
      const isMuted = this.audioManager.isMuted()

      const finalVolume = isMuted ? 0 : masterVolume * seVolume
      sound.setVolume(finalVolume)
    }

    sound.play()
  }
}
