import './style.css';
import typescriptLogo from './typescript.svg';
import viteLogo from '/vite.svg';
import { FizzBuzzCommand } from './application/FizzBuzzCommand';

// FizzBuzzアプリケーションの初期化
let currentCommand = new FizzBuzzCommand(1);

document.querySelector<HTMLDivElement>('#app')!.innerHTML = `
  <div>
    <a href="https://vite.dev" target="_blank">
      <img src="${viteLogo}" class="logo" alt="Vite logo" />
    </a>
    <a href="https://www.typescriptlang.org/" target="_blank">
      <img src="${typescriptLogo}" class="logo vanilla" alt="TypeScript logo" />
    </a>
    <h1>FizzBuzz Web Application</h1>
    
    <div class="card">
      <h2>FizzBuzz Type Selection</h2>
      <div class="type-selector">
        <button id="type1" class="type-btn active" data-type="1">Type 1 (Standard)</button>
        <button id="type2" class="type-btn" data-type="2">Type 2 (Numbers only)</button>
        <button id="type3" class="type-btn" data-type="3">Type 3 (FizzBuzz only for 15)</button>
      </div>
    </div>

    <div class="card">
      <h2>Single Number Test</h2>
      <div class="input-group">
        <input type="number" id="singleNumber" placeholder="Enter a number" value="15" min="1" max="1000">
        <button id="testSingle">Test</button>
      </div>
      <div id="singleResult" class="result"></div>
    </div>

    <div class="card">
      <h2>Generate FizzBuzz List</h2>
      <div class="input-group">
        <input type="number" id="maxNumber" placeholder="Max number" value="100" min="1" max="1000">
        <button id="generateList">Generate List</button>
        <button id="showStats">Show Statistics</button>
        <button id="showFizzBuzzOnly">Show FizzBuzz Only</button>
      </div>
      <div id="listResult" class="result"></div>
      <div id="statsResult" class="result"></div>
    </div>

    <p class="read-the-docs">
      Test different FizzBuzz implementations and see the results in real-time!
    </p>
  </div>
`;

// イベントリスナーの設定
function setupEventListeners() {
  // タイプ選択ボタン
  document.querySelectorAll('.type-btn').forEach(btn => {
    btn.addEventListener('click', (e) => {
      const target = e.target as HTMLButtonElement;
      const type = parseInt(target.dataset.type || '1');
      
      // ボタンの状態更新
      document.querySelectorAll('.type-btn').forEach(b => b.classList.remove('active'));
      target.classList.add('active');
      
      // コマンドを新しいタイプで更新
      currentCommand = new FizzBuzzCommand(type);
      
      // 結果をクリア
      clearResults();
      
      console.log(`FizzBuzz type changed to: ${type}`);
    });
  });

  // 単一数値テスト
  document.getElementById('testSingle')?.addEventListener('click', () => {
    const input = document.getElementById('singleNumber') as HTMLInputElement;
    const number = parseInt(input.value);
    
    if (isNaN(number) || number < 1) {
      showResult('singleResult', 'Please enter a valid number (1 or greater)', 'error');
      return;
    }
    
    try {
      const result = currentCommand.execute(number);
      showResult('singleResult', `${number} → "${result}"`, 'success');
    } catch (error) {
      showResult('singleResult', `Error: ${error}`, 'error');
    }
  });

  // リスト生成
  document.getElementById('generateList')?.addEventListener('click', () => {
    const input = document.getElementById('maxNumber') as HTMLInputElement;
    const maxNumber = parseInt(input.value);
    
    if (isNaN(maxNumber) || maxNumber < 1 || maxNumber > 1000) {
      showResult('listResult', 'Please enter a valid number (1-1000)', 'error');
      return;
    }
    
    try {
      const start = performance.now();
      const list = currentCommand.executeList(maxNumber);
      const end = performance.now();
      
      const displayList = maxNumber <= 50 ? list : 
        [...list.slice(0, 25), '...', ...list.slice(-25)];
      
      const resultHtml = `
        <div class="list-info">
          <p><strong>Generated ${maxNumber} items in ${(end - start).toFixed(2)}ms</strong></p>
          ${maxNumber > 50 ? '<p><em>Showing first 25 and last 25 items...</em></p>' : ''}
        </div>
        <div class="fizzbuzz-list">
          ${displayList.map((item, index) => {
            const actualIndex = index < 25 || maxNumber <= 50 ? index + 1 : 
              index === 25 ? '...' : maxNumber - (displayList.length - index - 1);
            return `<span class="fizzbuzz-item ${getItemClass(item)}">${actualIndex}: ${item}</span>`;
          }).join('')}
        </div>
      `;
      
      showResult('listResult', resultHtml, 'success');
    } catch (error) {
      showResult('listResult', `Error: ${error}`, 'error');
    }
  });

  // 統計情報表示
  document.getElementById('showStats')?.addEventListener('click', () => {
    const input = document.getElementById('maxNumber') as HTMLInputElement;
    const maxNumber = parseInt(input.value);
    
    if (isNaN(maxNumber) || maxNumber < 1 || maxNumber > 1000) {
      showResult('statsResult', 'Please enter a valid number (1-1000)', 'error');
      return;
    }
    
    try {
      currentCommand.executeList(maxNumber);
      const stats = currentCommand.getStatistics();
      
      const statsHtml = `
        <div class="statistics">
          <h3>Statistics for 1-${maxNumber}:</h3>
          <div class="stats-grid">
            <div class="stat-item">
              <span class="stat-label">Numbers:</span>
              <span class="stat-value">${stats.numbers}</span>
            </div>
            <div class="stat-item">
              <span class="stat-label">Fizz:</span>
              <span class="stat-value">${stats.fizz}</span>
            </div>
            <div class="stat-item">
              <span class="stat-label">Buzz:</span>
              <span class="stat-value">${stats.buzz}</span>
            </div>
            <div class="stat-item">
              <span class="stat-label">FizzBuzz:</span>
              <span class="stat-value">${stats.fizzBuzz}</span>
            </div>
          </div>
          <div class="total">
            <strong>Total: ${stats.numbers + stats.fizz + stats.buzz + stats.fizzBuzz}</strong>
          </div>
        </div>
      `;
      
      showResult('statsResult', statsHtml, 'success');
    } catch (error) {
      showResult('statsResult', `Error: ${error}`, 'error');
    }
  });

  // FizzBuzzのみ表示
  document.getElementById('showFizzBuzzOnly')?.addEventListener('click', () => {
    const input = document.getElementById('maxNumber') as HTMLInputElement;
    const maxNumber = parseInt(input.value);
    
    if (isNaN(maxNumber) || maxNumber < 1 || maxNumber > 1000) {
      showResult('listResult', 'Please enter a valid number (1-1000)', 'error');
      return;
    }
    
    try {
      currentCommand.executeList(maxNumber);
      const fizzBuzzOnly = currentCommand.getFizzBuzzOnly();
      
      if (fizzBuzzOnly.length === 0) {
        showResult('listResult', 'No FizzBuzz items found in the range.', 'info');
        return;
      }
      
      const resultHtml = `
        <div class="list-info">
          <p><strong>Found ${fizzBuzzOnly.length} FizzBuzz items:</strong></p>
        </div>
        <div class="fizzbuzz-list">
          ${fizzBuzzOnly.map((item, index) => 
            `<span class="fizzbuzz-item fizzbuzz">${(index + 1) * 15}: ${item}</span>`
          ).join('')}
        </div>
      `;
      
      showResult('listResult', resultHtml, 'success');
    } catch (error) {
      showResult('listResult', `Error: ${error}`, 'error');
    }
  });
}

// ヘルパー関数
function showResult(elementId: string, content: string, type: 'success' | 'error' | 'info') {
  const element = document.getElementById(elementId);
  if (element) {
    element.innerHTML = content;
    element.className = `result ${type}`;
  }
}

function clearResults() {
  ['singleResult', 'listResult', 'statsResult'].forEach(id => {
    const element = document.getElementById(id);
    if (element) {
      element.innerHTML = '';
      element.className = 'result';
    }
  });
}

function getItemClass(item: string): string {
  if (item === 'FizzBuzz') return 'fizzbuzz';
  if (item === 'Fizz') return 'fizz';
  if (item === 'Buzz') return 'buzz';
  return 'number';
}

// 初期化
setupEventListeners();

