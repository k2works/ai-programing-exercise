import { Game } from './Game'

// ぷよぷよゲームのメイン処理
console.log('ぷよぷよゲームを開始します!')

// Canvas要素の取得とゲーム初期化
const canvas = document.getElementById('game-canvas') as HTMLCanvasElement
if (canvas) {
  try {
    const game = new Game(canvas)
    let gameStarted = false
    
    // DOM要素の取得
    const scoreElement = document.getElementById('score')
    const allClearElement = document.getElementById('all-clear-count')
    const gameOverElement = document.getElementById('game-over')
    const gameOverTitleElement = document.getElementById('game-over-title')
    const finalScoreElement = document.getElementById('final-score')
    const finalAllClearElement = document.getElementById('final-all-clear')
    const playTimeElement = document.getElementById('play-time')
    const restartButton = document.getElementById('restart-button')
    
    // ゲーム開始
    function startGame() {
      game.start()
      gameStarted = true
      
      // UI初期化
      updateUI()
      hideGameOver()
    }
    
    // UI更新関数
    function updateUI() {
      if (scoreElement) {
        scoreElement.textContent = `スコア: ${game.getScore()}`
      }
      if (allClearElement) {
        allClearElement.textContent = `全消し: ${game.getAllClearCount()}回`
      }
    }
    
    // ゲームオーバー表示
    function showGameOver() {
      if (!gameOverElement || !gameOverTitleElement || !finalScoreElement || 
          !finalAllClearElement || !playTimeElement) return
      
      const gameOverInfo = game.getGameOverInfo()
      const gameOverEffect = game.getGameOverEffect()
      
      // タイトルとスタイルを設定
      gameOverTitleElement.textContent = gameOverEffect.message
      gameOverTitleElement.style.color = gameOverEffect.color
      
      // 最終結果を表示
      finalScoreElement.textContent = `最終スコア: ${gameOverInfo.finalScore}`
      finalAllClearElement.textContent = `全消し: ${gameOverInfo.allClearCount}回`
      playTimeElement.textContent = `プレイ時間: ${gameOverInfo.playTime}秒`
      
      // アニメーション効果
      gameOverElement.style.display = 'block'
      gameOverElement.style.animation = gameOverEffect.animation === 'bounce' ? 
        'bounce 0.5s ease-in-out' : 'fadeIn 0.5s ease-in-out'
    }
    
    // ゲームオーバー画面を隠す
    function hideGameOver() {
      if (gameOverElement) {
        gameOverElement.style.display = 'none'
      }
    }
    
    // リスタートボタンのイベントリスナー
    if (restartButton) {
      restartButton.addEventListener('click', () => {
        startGame()
      })
    }
    
    // キーボード入力の設定
    document.addEventListener('keydown', (event) => {
      if (gameStarted && game.isRunning()) {
        game.handleInput(event.code)
      }
      
      // Rキーでリスタート
      if (event.code === 'KeyR' && !game.isRunning()) {
        startGame()
      }
      
      // テスト用：Gキーでゲームオーバーを強制発生
      if (event.code === 'KeyG' && game.isRunning()) {
        const stage = game.getStage()
        // スタート位置を塞いでゲームオーバーを発生させる
        stage.setCell(2, 1, 1)
        stage.setCell(2, 0, 1)
      }
    })

    // キーを離した時の処理
    document.addEventListener('keyup', (event) => {
      if (gameStarted && game.isRunning()) {
        game.handleKeyUp(event.code)
      }
    })

    // ゲームループの開始
    let lastGameOverCheck = false
    setInterval(() => {
      if (gameStarted) {
        game.update()
        updateUI()
        
        // ゲームオーバーチェック
        const currentGameOverState = !game.isRunning()
        if (currentGameOverState && !lastGameOverCheck) {
          // ゲームオーバーになった瞬間
          setTimeout(() => {
            showGameOver()
          }, 500) // 少し遅延を入れて自然な演出
        }
        lastGameOverCheck = currentGameOverState
      }
    }, 1000 / 60) // 60FPS
    
    // 初回ゲーム開始
    startGame()
    
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