import React, { useMemo } from 'react';
import { GamePage } from './presentation/pages/GamePage';
import { GameServiceImpl } from './application/services/GameService';
import { KeyboardInputHandler } from './infrastructure/input/KeyboardInputHandler';
import { TouchInputHandler } from './infrastructure/input/TouchInputHandler';
import { LocalStorageGameRepository } from './infrastructure/repositories/LocalStorageGameRepository';
import { CanvasGameRenderer } from './infrastructure/rendering/CanvasGameRenderer';
import { SimpleDependencyContainer } from './application/ports/DependencyContainer';
import type { InputHandler } from './application/ports/InputHandler';

/**
 * メインアプリケーションコンポーネント
 * 依存性注入とアプリケーションの初期化を行う
 */
const App: React.FC = () => {
  // 依存性の初期化（メモ化してパフォーマンスを最適化）
  const dependencies = useMemo(() => {
    // リポジトリの初期化
    const gameRepository = new LocalStorageGameRepository();
    
    // レンダラーの初期化
    const gameRenderer = new CanvasGameRenderer();
    
    // 入力ハンドラーの初期化
    const keyboardInputHandler = new KeyboardInputHandler();
    const touchInputHandler = new TouchInputHandler();
    
    // 複合入力ハンドラーの作成
    const inputHandler: InputHandler = {
      handleKeyboardInput: (event: KeyboardEvent) => {
        return keyboardInputHandler.handleKeyboardInput(event);
      },
      handleTouchInput: (event: TouchEvent) => {
        return touchInputHandler.handleTouchInput(event);
      },
      handleSwipeGesture: (gesture) => {
        return touchInputHandler.handleSwipeGesture(gesture);
      },
      isValidInput: (event: KeyboardEvent | TouchEvent) => {
        if (event instanceof KeyboardEvent) {
          return keyboardInputHandler.isValidInput(event);
        } else {
          return touchInputHandler.isValidInput(event);
        }
      },
      isInputEnabled: () => true,
      enableInput: () => {
        // 入力有効化処理
      },
      disableInput: () => {
        // 入力無効化処理
      },
    };

    // 依存性コンテナの初期化
    const container = new SimpleDependencyContainer();
    container.registerGameRepository(gameRepository);
    container.registerInputHandler(inputHandler);
    container.registerGameRenderer(gameRenderer);
    
    // ゲームサービスの初期化
    const gameService = new GameServiceImpl(container);

    return {
      gameService,
      inputHandler,
      gameRenderer,
    };
  }, []);

  return (
    <GamePage
      gameService={dependencies.gameService}
      inputHandler={dependencies.inputHandler}
      gameRenderer={dependencies.gameRenderer}
    />
  );
};

export default App;
