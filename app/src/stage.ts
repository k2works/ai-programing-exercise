/**
 * ゲーム盤面の管理と消去判定を行うクラス
 *
 * 責務:
 * - ゲームフィールドの状態管理
 * - ぷよの接続判定と消去処理
 * - 連鎖処理とスコア計算
 * - 全消し判定とボーナス処理
 * - ゲームオーバー判定
 * - ぷよの着地処理
 */
export class Stage {
  private static readonly FIELD_WIDTH = 6
  private static readonly FIELD_HEIGHT = 13
  private static readonly ZENKESHI_BONUS = 3600

  // スコア計算のボーナステーブル（テストケースに合わせて調整）
  private static readonly CHAIN_BONUS = [
    0, 8, 16, 32, 64, 96, 128, 160, 192, 224, 256, 288, 320, 352, 384, 416, 448, 480, 512,
  ]

  private static readonly PIECE_BONUS = [0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 6]

  private static readonly COLOR_BONUS = [0, 0, 4, 8, 8]

  private field: number[][]
  private score = 0

  // 演出管理
  private isZenkeshiEffectActiveFlag = false
  private isGameOverEffectActiveFlag = false

  constructor() {
    // ゲームフィールドの初期化
    this.field = Array(Stage.FIELD_HEIGHT)
      .fill(null)
      .map(() => Array(Stage.FIELD_WIDTH).fill(0))
  }

  /**
   * フィールドの状態を取得する
   */
  getField(): number[][] {
    return this.field
  }

  /**
   * フィールドをリセットする
   */
  resetField(): void {
    this.field = Array(Stage.FIELD_HEIGHT)
      .fill(null)
      .map(() => Array(Stage.FIELD_WIDTH).fill(0))
  }

  /**
   * スコアを取得する
   */
  getScore(): number {
    return this.score
  }

  /**
   * スコアをリセットする
   */
  resetScore(): void {
    this.score = 0
  }

  /**
   * スコアを加算する
   */
  addScore(points: number): void {
    this.score += points
  }

  /**
   * アクティブぷよを盤面に着地させる
   */
  landActivePuyo(
    puyoPositions: Array<{ x: number; y: number }>,
    color1: number,
    color2: number
  ): void {
    if (puyoPositions.length >= 2) {
      this.field[puyoPositions[0].y][puyoPositions[0].x] = color1
      this.field[puyoPositions[1].y][puyoPositions[1].x] = color2
    }
  }

  /**
   * 指定した位置が空きかどうかをチェック
   */
  isPositionEmpty(x: number, y: number): boolean {
    // フィールドの境界チェック
    if (x < 0 || x >= Stage.FIELD_WIDTH || y < 0 || y >= Stage.FIELD_HEIGHT) {
      return false
    }

    // フィールドの占有チェック
    return this.field[y][x] === 0
  }

  /**
   * 複数の位置が全て空きかどうかをチェック
   */
  arePositionsValid(positions: Array<{ x: number; y: number }>): boolean {
    for (const pos of positions) {
      if (!this.isPositionEmpty(pos.x, pos.y)) {
        return false
      }
    }
    return true
  }

  /**
   * ぷよの接続判定: 指定した位置から同じ色で接続されたぷよをすべて検出
   */
  findConnectedPuyos(startX: number, startY: number): Array<{ x: number; y: number }> {
    const connected: Array<{ x: number; y: number }> = []
    const visited: boolean[][] = Array(Stage.FIELD_HEIGHT)
      .fill(null)
      .map(() => Array(Stage.FIELD_WIDTH).fill(false))

    const targetColor = this.field[startY][startX]
    if (targetColor === 0) {
      return connected // 空のセルからは何も返さない
    }

    // 深度優先探索で接続されたぷよを検出
    this.dfsConnectedPuyos(startX, startY, targetColor, visited, connected)

    return connected
  }

  /**
   * 深度優先探索で同じ色のぷよを再帰的に検出
   */
  private dfsConnectedPuyos(
    x: number,
    y: number,
    targetColor: number,
    visited: boolean[][],
    connected: Array<{ x: number; y: number }>
  ): void {
    // 境界チェック
    if (x < 0 || x >= Stage.FIELD_WIDTH || y < 0 || y >= Stage.FIELD_HEIGHT) {
      return
    }

    // 訪問済みまたは異なる色の場合はスキップ
    if (visited[y][x] || this.field[y][x] !== targetColor) {
      return
    }

    // 現在位置を訪問済みとして接続リストに追加
    visited[y][x] = true
    connected.push({ x, y })

    // 隣接する4方向を再帰的に探索
    this.dfsConnectedPuyos(x + 1, y, targetColor, visited, connected) // 右
    this.dfsConnectedPuyos(x - 1, y, targetColor, visited, connected) // 左
    this.dfsConnectedPuyos(x, y + 1, targetColor, visited, connected) // 下
    this.dfsConnectedPuyos(x, y - 1, targetColor, visited, connected) // 上
  }

  /**
   * 4つ以上つながった消去対象のぷよグループを検出
   */
  findEliminateGroups(): Array<Array<{ x: number; y: number }>> {
    const eliminateGroups: Array<Array<{ x: number; y: number }>> = []
    const visited: boolean[][] = Array(Stage.FIELD_HEIGHT)
      .fill(null)
      .map(() => Array(Stage.FIELD_WIDTH).fill(false))

    // フィールド全体をスキャンして4つ以上の接続グループを検出
    for (let y = 0; y < Stage.FIELD_HEIGHT; y++) {
      for (let x = 0; x < Stage.FIELD_WIDTH; x++) {
        // 空でない、かつまだ訪問していないセルから開始
        if (this.field[y][x] !== 0 && !visited[y][x]) {
          const connectedPuyos = this.findConnectedPuyos(x, y)

          // 4つ以上のグループは消去対象
          if (connectedPuyos.length >= 4) {
            eliminateGroups.push(connectedPuyos)
          }

          // 訪問済みにマーク（重複検出を避ける）
          for (const puyo of connectedPuyos) {
            visited[puyo.y][puyo.x] = true
          }
        }
      }
    }

    return eliminateGroups
  }

  /**
   * ぷよの消去処理: 4つ以上つながったぷよを実際に消去する
   */
  eliminatePuyos(): Array<Array<{ x: number; y: number }>> {
    // 消去対象のグループを検出
    const eliminateGroups = this.findEliminateGroups()

    // 検出された各グループのぷよを消去（フィールドから0にする）
    for (const group of eliminateGroups) {
      for (const puyo of group) {
        this.field[puyo.y][puyo.x] = 0
      }
    }

    return eliminateGroups
  }

  /**
   * 消去後の落下処理: 空いたスペースに上のぷよを落下させる
   */
  dropAfterElimination(): boolean {
    let hasDropped = false

    // 各列ごとに処理
    for (let x = 0; x < Stage.FIELD_WIDTH; x++) {
      // 空でないぷよを上から下の順で収集
      const column: number[] = []

      // 列全体を上から下にスキャンして、空でないぷよを収集
      for (let y = 0; y < Stage.FIELD_HEIGHT; y++) {
        if (this.field[y][x] !== 0) {
          column.push(this.field[y][x])
        }
      }

      // 下から連続して配置されているかチェック
      let needsReorder = false
      for (let i = 0; i < column.length; i++) {
        const expectedY = Stage.FIELD_HEIGHT - 1 - i
        const expectedValue = column[column.length - 1 - i]
        if (this.field[expectedY][x] !== expectedValue) {
          needsReorder = true
          break
        }
      }

      // 必要であれば列を再配置
      if (needsReorder) {
        hasDropped = true

        // 列をクリア
        for (let y = 0; y < Stage.FIELD_HEIGHT; y++) {
          this.field[y][x] = 0
        }

        // 下から順番に配置
        for (let i = 0; i < column.length; i++) {
          const targetY = Stage.FIELD_HEIGHT - 1 - i
          this.field[targetY][x] = column[column.length - 1 - i]
        }
      }
    }

    return hasDropped
  }

  /**
   * 消去と落下を統合して実行
   */
  eliminateAndDrop(): { eliminated: Array<Array<{ x: number; y: number }>>; dropped: boolean } {
    const eliminatedGroups = this.eliminatePuyos()
    const dropped = eliminatedGroups.length > 0 ? this.dropAfterElimination() : false

    return {
      eliminated: eliminatedGroups,
      dropped: dropped,
    }
  }

  /**
   * 連鎖処理を実行するメソッド
   */
  processChain(): { chains: number; totalEliminated: number } {
    let chainCount = 0
    let totalEliminated = 0

    // 連鎖が続く限り繰り返し処理
    while (true) {
      // 消去処理を実行
      const eliminatedGroups = this.eliminatePuyos()

      // 消去対象がない場合は連鎖終了
      if (eliminatedGroups.length === 0) {
        break
      }

      // 連鎖カウントと消去数を更新
      chainCount++
      for (const group of eliminatedGroups) {
        totalEliminated += group.length
      }

      // 落下処理を実行
      this.dropAfterElimination()

      // 連鎖の無限ループを防ぐため、最大10回まで
      if (chainCount >= 10) {
        break
      }
    }

    return {
      chains: chainCount,
      totalEliminated: totalEliminated,
    }
  }

  /**
   * 連鎖スコアを計算するメソッド
   */
  calculateScore(chainNumber: number, piecesEliminated: number, colors: number): number {
    const chainBonus = Stage.CHAIN_BONUS[Math.min(chainNumber, Stage.CHAIN_BONUS.length - 1)]
    const pieceBonus = Stage.PIECE_BONUS[Math.min(piecesEliminated, Stage.PIECE_BONUS.length - 1)]
    const colorBonus = Stage.COLOR_BONUS[Math.min(colors, Stage.COLOR_BONUS.length - 1)]

    const scale = chainBonus + pieceBonus + colorBonus
    return piecesEliminated * 10 * scale
  }

  /**
   * 連鎖処理とスコア計算を統合したメソッド
   */
  processChainWithScore(): { chains: number; totalScore: number; totalEliminated: number } {
    let chainCount = 0
    let totalScore = 0
    let totalEliminated = 0

    // 連鎖が続く限り繰り返し処理
    while (true) {
      // 消去処理を実行
      const eliminatedGroups = this.eliminatePuyos()

      // 消去対象がない場合は連鎖終了
      if (eliminatedGroups.length === 0) {
        break
      }

      // 連鎖カウントを更新
      chainCount++

      // 各グループのスコアを計算
      let chainEliminated = 0
      const colors = eliminatedGroups.length // 消去されたグループ数 = 色数

      for (const group of eliminatedGroups) {
        chainEliminated += group.length
      }

      totalEliminated += chainEliminated

      // この連鎖のスコアを計算して加算
      const chainScore = this.calculateScore(chainCount, chainEliminated, colors)
      totalScore += chainScore

      this.dropAfterElimination()

      // 連鎖の無限ループを防ぐため、最大10回まで
      if (chainCount >= 10) {
        break
      }
    }

    // 全消しボーナスの判定と加算
    let zenkeshiBonus = 0
    if (chainCount > 0 && this.isZenkeshi()) {
      zenkeshiBonus = Stage.ZENKESHI_BONUS
      totalScore += zenkeshiBonus
      // 全消し演出を開始
      this.startZenkeshiEffect()
    }

    // ゲームのスコアに加算
    this.addScore(totalScore)

    return {
      chains: chainCount,
      totalScore: totalScore,
      totalEliminated: totalEliminated,
    }
  }

  /**
   * 全消し判定: 盤面上にぷよが残っていないかをチェック
   */
  isZenkeshi(): boolean {
    // フィールド全体をスキャンして、空でないセルがあるかチェック
    for (let y = 0; y < Stage.FIELD_HEIGHT; y++) {
      for (let x = 0; x < Stage.FIELD_WIDTH; x++) {
        if (this.field[y][x] !== 0) {
          return false // ぷよが残っている場合は全消しではない
        }
      }
    }
    return true // すべてのセルが空の場合は全消し
  }

  /**
   * 全消しボーナス値を取得
   */
  getZenkeshiBonus(): number {
    return Stage.ZENKESHI_BONUS
  }

  /**
   * 全消し演出が有効かどうかを取得
   */
  isZenkeshiEffectActive(): boolean {
    return this.isZenkeshiEffectActiveFlag
  }

  /**
   * 全消し演出を開始
   */
  private startZenkeshiEffect(): void {
    this.isZenkeshiEffectActiveFlag = true
  }

  /**
   * 全消し演出を停止
   */
  stopZenkeshiEffect(): void {
    this.isZenkeshiEffectActiveFlag = false
  }

  /**
   * ゲームオーバー判定: 新しいぷよを配置できるかをチェック
   */
  isGameOver(
    calculatePuyoPositions: (
      x: number,
      y: number,
      direction: number
    ) => Array<{ x: number; y: number }>
  ): boolean {
    // 新しいぷよのデフォルト配置位置（x=2, y=0）をチェック
    const spawnX = 2
    const spawnY = 0
    const spawnDirection = 0 // デフォルトの縦配置

    // 新しいぷよの2つの位置を取得
    const positions = calculatePuyoPositions(spawnX, spawnY, spawnDirection)

    // すべての位置が空かどうかチェック
    for (const pos of positions) {
      // フィールドの境界チェック
      if (pos.x < 0 || pos.x >= Stage.FIELD_WIDTH || pos.y < 0 || pos.y >= Stage.FIELD_HEIGHT) {
        return true // 境界外なのでゲームオーバー
      }

      // フィールドの占有チェック
      if (this.field[pos.y] && this.field[pos.y][pos.x] !== 0) {
        return true // 占有されているのでゲームオーバー
      }
    }

    return false // すべての位置が空なのでゲームオーバーではない
  }

  /**
   * ゲームオーバー演出が有効かどうかを取得
   */
  isGameOverEffectActive(): boolean {
    return this.isGameOverEffectActiveFlag
  }

  /**
   * ゲームオーバー演出を開始
   */
  triggerGameOver(): void {
    this.isGameOverEffectActiveFlag = true
  }

  /**
   * ゲームオーバー演出を停止
   */
  stopGameOverEffect(): void {
    this.isGameOverEffectActiveFlag = false
  }

  /**
   * 演出を全て停止する
   */
  stopAllEffects(): void {
    this.isZenkeshiEffectActiveFlag = false
    this.isGameOverEffectActiveFlag = false
  }
}
