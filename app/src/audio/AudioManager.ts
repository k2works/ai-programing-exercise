import Phaser from 'phaser'

/**
 * 音量設定の定義
 */
export interface VolumeSettings {
  master: number
  bgm: number
  se: number
}

/**
 * オーディオマネージャー
 * ゲーム内の音響システムを統括管理
 */
export class AudioManager {
  private scene: Phaser.Scene
  private masterVolume: number = 1.0
  private bgmVolume: number = 0.7
  private seVolume: number = 0.8
  private muted: boolean = false

  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  private currentBgm: any = null
  private currentBgmKey: string | null = null
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  private activeSounds: any[] = []

  // 音量変更コールバック
  public onVolumeChange?: (volumes: VolumeSettings) => void

  constructor(scene: Phaser.Scene) {
    this.scene = scene
  }

  /**
   * マスター音量を取得
   */
  getMasterVolume(): number {
    return this.masterVolume
  }

  /**
   * BGM音量を取得
   */
  getBgmVolume(): number {
    return this.bgmVolume
  }

  /**
   * SE音量を取得
   */
  getSeVolume(): number {
    return this.seVolume
  }

  /**
   * マスター音量を設定
   */
  setMasterVolume(volume: number): void {
    this.masterVolume = this.clampVolume(volume)
    this.updateCurrentSoundsVolume()
    this.notifyVolumeChange()
  }

  /**
   * BGM音量を設定
   */
  setBgmVolume(volume: number): void {
    this.bgmVolume = this.clampVolume(volume)
    this.updateBgmVolume()
    this.notifyVolumeChange()
  }

  /**
   * SE音量を設定
   */
  setSeVolume(volume: number): void {
    this.seVolume = this.clampVolume(volume)
    this.notifyVolumeChange()
  }

  /**
   * 音量を0-1の範囲に制限
   */
  private clampVolume(volume: number): number {
    return Math.max(0, Math.min(1, volume))
  }

  /**
   * 音量変更通知
   */
  private notifyVolumeChange(): void {
    if (this.onVolumeChange) {
      this.onVolumeChange({
        master: this.masterVolume,
        bgm: this.bgmVolume,
        se: this.seVolume,
      })
    }
  }

  /**
   * BGMを再生
   */
  playBgm(key: string): void {
    // 同じBGMが再生中の場合は何もしない
    if (this.currentBgmKey === key && this.currentBgm?.isPlaying) {
      return
    }

    // 現在のBGMを停止
    this.stopBgm()

    // 新しいBGMを再生
    this.currentBgm = this.scene.sound.add(key)
    this.currentBgmKey = key

    this.currentBgm.setLoop(true)
    this.updateBgmVolume()
    this.currentBgm.play()
  }

  /**
   * BGMを停止
   */
  stopBgm(): void {
    if (this.currentBgm) {
      this.currentBgm.stop()
      this.currentBgm = null
      this.currentBgmKey = null
    }
  }

  /**
   * BGMを一時停止
   */
  pauseBgm(): void {
    if (this.currentBgm) {
      this.currentBgm.pause()
    }
  }

  /**
   * BGMを再開
   */
  resumeBgm(): void {
    if (this.currentBgm) {
      this.currentBgm.resume()
    }
  }

  /**
   * BGM音量を更新
   */
  private updateBgmVolume(): void {
    if (this.currentBgm) {
      const volume = this.muted ? 0 : this.masterVolume * this.bgmVolume
      this.currentBgm.setVolume(volume)
    }
  }

  /**
   * SEを再生
   */
  playSe(key: string): void {
    const sound = this.scene.sound.add(key)
    const volume = this.muted ? 0 : this.masterVolume * this.seVolume

    sound.setVolume(volume)
    sound.play()

    // 再生完了時に配列から削除
    sound.once('complete', () => {
      this.removeSoundFromActive(sound)
    })

    this.activeSounds.push(sound)
  }

  /**
   * アクティブサウンドリストから音を削除
   */
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  private removeSoundFromActive(sound: any): void {
    const index = this.activeSounds.indexOf(sound)
    if (index > -1) {
      this.activeSounds.splice(index, 1)
    }
  }

  /**
   * 全てのSEを停止
   */
  stopAllSe(): void {
    this.scene.sound.stopAll()
    this.activeSounds = []
  }

  /**
   * BGMをフェードイン
   */
  fadeInBgm(key: string, duration: number): void {
    this.playBgm(key)

    if (this.currentBgm) {
      // 音量を0から開始してフェードイン
      this.currentBgm.setVolume(0)

      // Phaserのトゥイーンを使用してフェードイン
      this.scene.tweens.add({
        targets: this.currentBgm,
        volume: this.muted ? 0 : this.masterVolume * this.bgmVolume,
        duration: duration,
        ease: 'Linear',
      })
    }
  }

  /**
   * BGMをフェードアウト
   */
  fadeOutBgm(duration: number): Promise<void> {
    return new Promise((resolve) => {
      if (!this.currentBgm) {
        resolve()
        return
      }

      this.scene.tweens.add({
        targets: this.currentBgm,
        volume: 0,
        duration: duration,
        ease: 'Linear',
        onComplete: () => {
          this.stopBgm()
          resolve()
        },
      })
    })
  }

  /**
   * 現在の音をすべて更新
   */
  private updateCurrentSoundsVolume(): void {
    this.updateBgmVolume()

    // アクティブなSEの音量も更新
    this.activeSounds.forEach((sound) => {
      const volume = this.muted ? 0 : this.masterVolume * this.seVolume
      sound.setVolume(volume)
    })
  }

  /**
   * ミュート状態を設定
   */
  setMuted(muted: boolean): void {
    this.muted = muted
    this.updateCurrentSoundsVolume()
  }

  /**
   * ミュート状態を取得
   */
  isMuted(): boolean {
    return this.muted
  }

  /**
   * オーディオマネージャーを破棄
   */
  destroy(): void {
    this.scene.sound.stopAll()
    this.currentBgm = null
    this.currentBgmKey = null
    this.activeSounds = []
  }
}
