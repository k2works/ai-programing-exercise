import type { Puyo } from './Puyo';
import type { Position } from '../types/Position';
import { isValidPosition, createPosition } from '../types/Position';
import { createPuyo } from './Puyo';

/**
 * ゲームフィールドのインターフェース
 * 12行×6列のぷよぷよフィールドを表現
 */
export interface GameField {
  readonly width: number;
  readonly height: number;
  readonly puyos: ReadonlyArray<ReadonlyArray<Puyo | null>>;
}

/**
 * 空のゲームフィールドを作成する
 * @returns 12行×6列の空のゲームフィールド
 */
export const createGameField = (): GameField => {
  const puyos: (Puyo | null)[][] = [];
  
  for (let y = 0; y < 12; y++) {
    const row: (Puyo | null)[] = [];
    for (let x = 0; x < 6; x++) {
      row.push(null);
    }
    puyos.push(Object.freeze(row));
  }

  return Object.freeze({
    width: 6,
    height: 12,
    puyos: Object.freeze(puyos),
  });
};

/**
 * 指定された位置にぷよを配置した新しいフィールドを作成する
 * @param field 元のゲームフィールド
 * @param puyo 配置するぷよ
 * @param position 配置する位置
 * @returns ぷよが配置された新しいフィールド
 */
export const placePuyo = (field: GameField, puyo: Puyo, position: Position): GameField => {
  if (!isValidPosition(position)) {
    throw new Error('Invalid position');
  }

  const newPuyos = field.puyos.map((row, y) =>
    Object.freeze(
      row.map((cell, x) => {
        if (x === position.x && y === position.y) {
          return createPuyo(puyo.id, puyo.color, position, puyo.isFixed);
        }
        return cell;
      })
    )
  );

  return Object.freeze({
    ...field,
    puyos: Object.freeze(newPuyos),
  });
};

/**
 * 指定された位置のぷよを削除した新しいフィールドを作成する
 * @param field 元のゲームフィールド
 * @param positions 削除する位置の配列
 * @returns ぷよが削除された新しいフィールド
 */
export const removePuyos = (field: GameField, positions: ReadonlyArray<Position>): GameField => {
  const newPuyos = field.puyos.map((row, y) =>
    Object.freeze(
      row.map((cell, x) => {
        const shouldRemove = positions.some(pos => pos.x === x && pos.y === y);
        return shouldRemove ? null : cell;
      })
    )
  );

  return Object.freeze({
    ...field,
    puyos: Object.freeze(newPuyos),
  });
};

/**
 * 重力を適用してぷよを落下させた新しいフィールドを作成する
 * @param field 元のゲームフィールド
 * @returns 重力が適用された新しいフィールド
 */
export const applyGravity = (field: GameField): GameField => {
  const newPuyos: (Puyo | null)[][] = [];
  
  // 各行を初期化
  for (let y = 0; y < field.height; y++) {
    const row: (Puyo | null)[] = [];
    for (let x = 0; x < field.width; x++) {
      row.push(null);
    }
    newPuyos.push(row);
  }

  // 各列について重力を適用
  for (let x = 0; x < field.width; x++) {
    const columnPuyos: Puyo[] = [];
    
    // 列のぷよを収集（上から下へ）
    for (let y = 0; y < field.height; y++) {
      const puyo = field.puyos[y]![x];
      if (puyo !== null) {
        columnPuyos.push(puyo);
      }
    }
    
    // 底から順番にぷよを配置
    for (let i = 0; i < columnPuyos.length; i++) {
      const puyo = columnPuyos[i]!;
      const newY = field.height - 1 - i;
      const newPosition = createPosition(x, newY);
      newPuyos[newY]![x] = createPuyo(puyo.id, puyo.color, newPosition, puyo.isFixed);
    }
  }

  // 配列を不変にする
  const frozenPuyos = newPuyos.map(row => Object.freeze(row));

  return Object.freeze({
    ...field,
    puyos: Object.freeze(frozenPuyos),
  });
};

/**
 * 隣接する4方向の位置を取得する
 * @param position 基準位置
 * @returns 隣接する位置の配列
 */
const getNeighborPositions = (position: Position): Position[] => [
  createPosition(position.x - 1, position.y), // 左
  createPosition(position.x + 1, position.y), // 右
  createPosition(position.x, position.y - 1), // 上
  createPosition(position.x, position.y + 1), // 下
];

/**
 * 位置が既に訪問済みかチェックする
 * @param visited 訪問済み位置のSet
 * @param position チェックする位置
 * @returns 訪問済みの場合true
 */
const isVisited = (visited: Set<string>, position: Position): boolean => {
  const posKey = `${position.x},${position.y}`;
  return visited.has(posKey);
};

/**
 * 位置を訪問済みとしてマークする
 * @param visited 訪問済み位置のSet
 * @param position マークする位置
 */
const markAsVisited = (visited: Set<string>, position: Position): void => {
  const posKey = `${position.x},${position.y}`;
  visited.add(posKey);
};

/**
 * 現在の位置のぷよが開始ぷよと同じ色かチェックする
 * @param field ゲームフィールド
 * @param position チェックする位置
 * @param startPuyo 開始ぷよ
 * @returns 同じ色の場合true
 */
const isSameColorPuyo = (field: GameField, position: Position, startPuyo: Puyo): boolean => {
  const currentPuyo = field.puyos[position.y]?.[position.x];
  return currentPuyo !== null && currentPuyo.color === startPuyo.color;
};

/**
 * 隣接する有効な未訪問位置をキューに追加する
 * @param queue 処理キュー
 * @param visited 訪問済み位置のSet
 * @param currentPos 現在の位置
 */
const addValidNeighborsToQueue = (queue: Position[], visited: Set<string>, currentPos: Position): void => {
  const neighbors = getNeighborPositions(currentPos);
  
  for (const neighbor of neighbors) {
    if (isValidPosition(neighbor) && !isVisited(visited, neighbor)) {
      queue.push(neighbor);
    }
  }
};

/**
 * 指定された位置から同じ色で接続されたぷよを見つける
 * @param field ゲームフィールド
 * @param startPosition 開始位置
 * @returns 接続されたぷよの位置の配列
 */
export const findConnectedPuyos = (field: GameField, startPosition: Position): ReadonlyArray<Position> => {
  const startPuyo = field.puyos[startPosition.y]?.[startPosition.x];
  if (!startPuyo) {
    return [];
  }

  const visited = new Set<string>();
  const connected: Position[] = [];
  const queue: Position[] = [startPosition];

  while (queue.length > 0) {
    const currentPos = queue.shift()!;
    
    if (isVisited(visited, currentPos)) {
      continue;
    }
    
    markAsVisited(visited, currentPos);
    
    if (!isSameColorPuyo(field, currentPos, startPuyo)) {
      continue;
    }
    
    connected.push(currentPos);
    addValidNeighborsToQueue(queue, visited, currentPos);
  }

  return Object.freeze(connected);
};