/**
 * アプリケーション層のポートインターフェースのエクスポート
 * ヘキサゴナルアーキテクチャにおける依存性逆転の原則に従い、
 * 外部依存に対するインターフェースを定義
 */

export type { GameRepository } from './GameRepository';
export type { InputHandler, SwipeGesture, TouchGesture } from './InputHandler';
export { GameAction } from './InputHandler';
export type { GameRenderer, AnimationConfig } from './GameRenderer';
export type { DependencyContainer } from './DependencyContainer';
export { SimpleDependencyContainer } from './DependencyContainer';
