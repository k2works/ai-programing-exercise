export class Score {
  private score: number = 0

  getScore(): number {
    return this.score
  }

  addScore(points: number): void {
    this.score += points
  }

  reset(): void {
    this.score = 0
  }
}
