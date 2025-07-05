import './style.css'
import { FizzBuzz } from './FizzBuzz'

const fizzBuzz = new FizzBuzz()

document.querySelector<HTMLDivElement>('#app')!.innerHTML = `
  <div>
    <h1>FizzBuzz</h1>
    <div class="card">
      <button id="generateBtn" type="button">1から100までのFizzBuzzを生成</button>
    </div>
    <div id="result"></div>
  </div>
`

const generateBtn = document.querySelector<HTMLButtonElement>('#generateBtn')!
const resultDiv = document.querySelector<HTMLDivElement>('#result')!

generateBtn.addEventListener('click', () => {
  const results = fizzBuzz.generateList(1, 100)
  resultDiv.innerHTML = `
    <h2>結果:</h2>
    <div class="fizzbuzz-grid">
      ${results
        .map(
          (value, index) => `
        <div class="fizzbuzz-item ${getFizzBuzzClass(value)}">
          <span class="number">${index + 1}</span>
          <span class="result">${value}</span>
        </div>
      `
        )
        .join('')}
    </div>
  `
})

function getFizzBuzzClass(value: string): string {
  if (value === 'FizzBuzz') return 'fizzbuzz'
  if (value === 'Fizz') return 'fizz'
  if (value === 'Buzz') return 'buzz'
  return 'number'
}
