import './style.css'

document.querySelector<HTMLDivElement>('#app')!.innerHTML = `
  <div>
    <h1>Novel Game</h1>
    <div class="game-container">
      <div id="game-area">
        <p>ゲームを開始するには、スペースキーを押してください。</p>
      </div>
    </div>
  </div>
`
