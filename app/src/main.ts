import { Game } from './game'

// ゲームの初期化と開始
window.addEventListener('DOMContentLoaded', () => {
  const canvas = document.getElementById('game-canvas') as HTMLCanvasElement
  const scoreDisplay = document.getElementById('score-display') as HTMLElement

  if (!canvas || !scoreDisplay) {
    console.error('Required DOM elements not found')
    return
  }

  const game = new Game(canvas, scoreDisplay)
  game.start()
})
