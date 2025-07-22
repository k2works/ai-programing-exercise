import { Game } from "./game";

const game = new Game();

// 起動されたときに呼ばれる関数を登録する
window.addEventListener("load", () => {
  // まずステージを整える
  game.initialize();
  // ゲームを開始する
  game.loop();
});
