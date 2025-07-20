export class Score {
  private score: number = 0

  constructor() {
    // スコアの初期化
  }

  getScore(): number {
    return this.score
  }

  addScore(points: number): void {
    this.score += points
  }
}
