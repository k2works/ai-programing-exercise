import { Game } from './Game'

// ぷよぷよゲームのメイン処理
console.log('ぷよぷよゲームを開始します!')

// Canvas要素の取得とゲーム初期化
const canvas = document.getElementById('game-canvas') as HTMLCanvasElement
if (canvas) {
  try {
    const game = new Game(canvas)
    game.start()
    
    // スコア表示を更新
    const scoreElement = document.getElementById('score')
    if (scoreElement) {
      scoreElement.textContent = 'スコア: 0'
    }
    
    // キーボード入力の設定
    document.addEventListener('keydown', (event) => {
      game.handleInput(event.code)
    })

    // ゲームループの開始（暫定的にsetIntervalを使用）
    setInterval(() => {
      game.update()
    }, 1000 / 60) // 60FPS
    
    console.log('ゲームが正常に開始されました')
  } catch (error) {
    console.error('ゲームの初期化に失敗しました:', error)
    
    // フォールバック用の描画
    const ctx = canvas.getContext('2d')
    if (ctx) {
      ctx.fillStyle = '#ff0000'
      ctx.font = '16px Arial'
      ctx.fillText('ゲーム初期化エラー', 10, 30)
    }
  }
} else {
  console.error('Canvasが見つかりません')
}