import type { GameRepository } from './GameRepository';
import type { InputHandler } from './InputHandler';
import type { GameRenderer } from './GameRenderer';

/**
 * 依存性注入コンテナのインターフェース
 * ヘキサゴナルアーキテクチャにおける依存性逆転の原則を実現
 */
export interface DependencyContainer {
  /**
   * GameRepositoryの実装を登録する
   * @param repository GameRepositoryの実装
   */
  registerGameRepository(repository: GameRepository): void;

  /**
   * InputHandlerの実装を登録する
   * @param inputHandler InputHandlerの実装
   */
  registerInputHandler(inputHandler: InputHandler): void;

  /**
   * GameRendererの実装を登録する
   * @param renderer GameRendererの実装
   */
  registerGameRenderer(renderer: GameRenderer): void;

  /**
   * 登録されたGameRepositoryを取得する
   * @returns GameRepositoryの実装
   * @throws Error 登録されていない場合
   */
  getGameRepository(): GameRepository;

  /**
   * 登録されたInputHandlerを取得する
   * @returns InputHandlerの実装
   * @throws Error 登録されていない場合
   */
  getInputHandler(): InputHandler;

  /**
   * 登録されたGameRendererを取得する
   * @returns GameRendererの実装
   * @throws Error 登録されていない場合
   */
  getGameRenderer(): GameRenderer;

  /**
   * すべての依存関係がクリアされているかどうかを確認する
   * @returns すべてクリアされている場合はtrue
   */
  isEmpty(): boolean;

  /**
   * すべての依存関係をクリアする
   */
  clear(): void;
}

/**
 * シンプルな依存性注入コンテナの実装
 */
export class SimpleDependencyContainer implements DependencyContainer {
  private gameRepository?: GameRepository;
  private inputHandler?: InputHandler;
  private gameRenderer?: GameRenderer;

  registerGameRepository(repository: GameRepository): void {
    this.gameRepository = repository;
  }

  registerInputHandler(inputHandler: InputHandler): void {
    this.inputHandler = inputHandler;
  }

  registerGameRenderer(renderer: GameRenderer): void {
    this.gameRenderer = renderer;
  }

  getGameRepository(): GameRepository {
    if (!this.gameRepository) {
      throw new Error('GameRepository is not registered');
    }
    return this.gameRepository;
  }

  getInputHandler(): InputHandler {
    if (!this.inputHandler) {
      throw new Error('InputHandler is not registered');
    }
    return this.inputHandler;
  }

  getGameRenderer(): GameRenderer {
    if (!this.gameRenderer) {
      throw new Error('GameRenderer is not registered');
    }
    return this.gameRenderer;
  }

  isEmpty(): boolean {
    return !this.gameRepository && !this.inputHandler && !this.gameRenderer;
  }

  clear(): void {
    delete this.gameRepository;
    delete this.inputHandler;
    delete this.gameRenderer;
  }
}
