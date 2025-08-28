import './style.css'
import { PhaserGameConfig } from './phaser-config'

// Phaser3ゲームの初期化
const phaserConfig = new PhaserGameConfig()
const game = phaserConfig.createGame()

// ブラウザのウィンドウが閉じられる時にゲームを破棄
window.addEventListener('beforeunload', () => {
  if (game) {
    game.destroy(true)
  }
})

console.log('TypeScript Novel Game initialized with Phaser3')
