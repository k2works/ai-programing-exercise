import type { GameField } from '../models/GameField';
import type { Position } from '../types/Position';
import type { PuyoColor } from '../types/PuyoColor';
import { isValidPosition, createPosition } from '../types/Position';

/**
 * ぷよのマッチンググループを表すインターフェース
 */
export interface PuyoGroup {
  readonly color: PuyoColor;
  readonly positions: ReadonlyArray<Position>;
}

/**
 * ぷよのマッチング処理を行うサービス
 * 同じ色のぷよが4つ以上隣接している場合にマッチンググループとして検出する
 */
export class PuyoMatcher {
  /**
   * フィールド内のすべてのマッチンググループを検出する
   * @param field ゲームフィールド
   * @returns 4つ以上の同じ色のぷよで構成されるグループの配列
   */
  findMatchingGroups(field: GameField): ReadonlyArray<PuyoGroup> {
    const visited = new Set<string>();
    const matchingGroups: PuyoGroup[] = [];

    // フィールド全体をスキャン
    for (let y = 0; y < field.height; y++) {
      for (let x = 0; x < field.width; x++) {
        const position = createPosition(x, y);
        const positionKey = this.positionToKey(position);

        // 既に訪問済みまたは空のセルはスキップ
        if (visited.has(positionKey)) {
          continue;
        }

        const puyo = field.puyos[y]?.[x];
        if (!puyo) {
          continue;
        }

        // 接続されたぷよを検索
        const connectedPositions = this.findConnectedPuyos(field, position);

        // 接続されたぷよを訪問済みとしてマーク
        connectedPositions.forEach((pos) => {
          visited.add(this.positionToKey(pos));
        });

        // 4つ以上の場合はマッチンググループとして追加
        if (connectedPositions.length >= 4) {
          matchingGroups.push({
            color: puyo.color,
            positions: Object.freeze([...connectedPositions]),
          });
        }
      }
    }

    return Object.freeze(matchingGroups);
  }

  /**
   * 指定された位置から同じ色で接続されたぷよをすべて検索する
   * @param field ゲームフィールド
   * @param startPosition 開始位置
   * @returns 接続されたぷよの位置の配列
   */
  findConnectedPuyos(
    field: GameField,
    startPosition: Position
  ): ReadonlyArray<Position> {
    const startPuyo = field.puyos[startPosition.y]?.[startPosition.x];
    if (!startPuyo) {
      return [];
    }

    const visited = new Set<string>();
    const connected: Position[] = [];
    const queue: Position[] = [startPosition];

    while (queue.length > 0) {
      const currentPos = queue.shift()!;

      if (this.shouldSkipPosition(visited, currentPos)) {
        continue;
      }

      if (this.isSameColorPuyo(field, currentPos, startPuyo)) {
        connected.push(currentPos);
        this.addNeighborsToQueue(queue, visited, currentPos);
      }
    }

    return Object.freeze(connected);
  }

  /**
   * 位置をスキップすべきかどうかを判定する
   */
  private shouldSkipPosition(
    visited: Set<string>,
    position: Position
  ): boolean {
    const positionKey = this.positionToKey(position);
    if (visited.has(positionKey)) {
      return true;
    }
    visited.add(positionKey);
    return false;
  }

  /**
   * 指定された位置のぷよが開始ぷよと同じ色かどうかを判定する
   */
  private isSameColorPuyo(
    field: GameField,
    position: Position,
    startPuyo: { color: PuyoColor }
  ): boolean {
    const currentPuyo = field.puyos[position.y]?.[position.x];
    return currentPuyo !== null && currentPuyo?.color === startPuyo.color;
  }

  /**
   * 隣接する位置をキューに追加する
   */
  private addNeighborsToQueue(
    queue: Position[],
    visited: Set<string>,
    currentPos: Position
  ): void {
    const neighbors = this.getNeighborPositions(currentPos);
    for (const neighbor of neighbors) {
      if (
        isValidPosition(neighbor) &&
        !visited.has(this.positionToKey(neighbor))
      ) {
        queue.push(neighbor);
      }
    }
  }

  /**
   * 隣接する4方向の位置を取得する
   * @param position 基準位置
   * @returns 隣接する位置の配列
   */
  private getNeighborPositions(position: Position): Position[] {
    return [
      createPosition(position.x - 1, position.y), // 左
      createPosition(position.x + 1, position.y), // 右
      createPosition(position.x, position.y - 1), // 上
      createPosition(position.x, position.y + 1), // 下
    ];
  }

  /**
   * 位置をキー文字列に変換する
   * @param position 位置
   * @returns キー文字列
   */
  private positionToKey(position: Position): string {
    return `${position.x},${position.y}`;
  }
}
