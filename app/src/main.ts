// ぷよぷよゲームのメインファイル
// TDD開発の準備用の最小限実装

console.log('ぷよぷよゲームを開始します!')

// 基本的なゲームクラスの準備（後でTDDで実装）
export class Game {
  constructor() {
    console.log('Game initialized')
  }

  start(): void {
    console.log('Game started')
  }
}

// 開発環境確認のためのテンポラリー実装
const game = new Game()
game.start()

// Canvas要素の取得確認
const canvas = document.getElementById('game-canvas') as HTMLCanvasElement
if (canvas) {
  const ctx = canvas.getContext('2d')
  if (ctx) {
    // テスト用の描画
    ctx.fillStyle = '#ff0000'
    ctx.fillRect(10, 10, 50, 50)
    ctx.fillStyle = '#00ff00'
    ctx.fillRect(70, 10, 50, 50)
    ctx.fillStyle = '#0000ff'
    ctx.fillRect(130, 10, 50, 50)
    ctx.fillStyle = '#ffff00'
    ctx.fillRect(190, 10, 50, 50)
    
    ctx.fillStyle = '#ffffff'
    ctx.font = '16px Arial'
    ctx.fillText('環境構築完了！TDD開発準備完了', 50, 100)
  }
}